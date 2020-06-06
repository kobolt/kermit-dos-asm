org 0x100
bits 16
cpu 8086



COM1_BASE equ 0x3f8
COM1_THR  equ COM1_BASE + 0 ; Transmitter Holding Buffer
COM1_RBR  equ COM1_BASE + 0 ; Receiver Buffer
COM1_IER  equ COM1_BASE + 1 ; Interrupt Enable Register
COM1_FCR  equ COM1_BASE + 2 ; FIFO Control Register
COM1_IIR  equ COM1_BASE + 2 ; Interrupt Identification Register
COM1_LCR  equ COM1_BASE + 3 ; Line Control Register
COM1_LSR  equ COM1_BASE + 5 ; Line Status Register
COM1_DLL  equ COM1_BASE + 0 ; Divisor Latch Low Byte
COM1_DLH  equ COM1_BASE + 1 ; Divisor Latch High Byte

RECV_BUFFER_SIZE equ 100 ; As per Kermit documentation.
SEND_BUFFER_SIZE equ 16 ; No packets sent from this program exceed this.
PACKET_DATA_SIZE equ 94 ; Receive buffer size - 6 fields/terminator.



section .text
start:
  ; Set Baudrate on COM1 to 9600, divisor = 12:
  mov dx, COM1_LCR
  in al, dx
  or al, 0b10000000 ; Set Divisor Latch Access Bit (DLAB)
  out dx, al

  mov dx, COM1_DLL
  mov al, 0xc
  out dx, al
  
  mov dx, COM1_DLH
  mov al, 0
  out dx, al

  mov dx, COM1_LCR
  in al, dx
  and al, 0b01111111 ; Reset Divisor Latch Access Bit (DLAB)
  out dx, al

  ; Disable and clear FIFO on COM1, to put it in 8250 compatibility mode:
  mov dx, COM1_FCR
  mov al, 0b00000110 ; Clear both FIFOs.
  out dx, al

  ; Set mode on COM1 to 8 data bits, no parity and 1 stop bit:
  mov dx, COM1_LCR
  mov al, 0b00000011 ; 8-N-1
  out dx, al

  ; Enable interrupt bit on COM1:
  mov dx, COM1_IER
  in al, dx
  or al, 0b00000001 ; Enable Received Data Available Interrupt
  out dx, al

  ; Initialize sequence number:
  mov byte [send_seq_no], 0

  ; Initialize retransmit message in send buffer:
  mov byte [send_buffer],     0x01 ; 'MARK' Start marker (Ctrl-A)
  mov byte [send_buffer + 1], 35   ; 'LEN' Packet length of 3 (+ 32)
  mov byte [send_buffer + 2], 32   ; 'SEQ' Sequence number 0 (+ 32)
  mov byte [send_buffer + 3], 'N'  ; 'TYPE' Packet type 'N' meaning NAK
  mov byte [send_buffer + 4], 0x33 ; 'CHECK' Checksum 0x13 (+ 32)
  mov byte [send_buffer + 5], 0x13 ; Terminator (Carriage Return)
  mov word [send_buffer_index], 6

  ; Get Send Initialization packet, exchange parameters...

  mov dx, message_waiting
  call print_string
  call get_valid_packet

  mov byte al, [packet_type]
  cmp al, 'S' ; 'S' = Send Initiation
  je _handle_s_packet

  ; Simplified version of error message: "<?> Packet in S State"
  mov byte [packet_data], al
  mov byte [packet_data + 1], 'S'
  mov byte [packet_type], 'E' ; 'E' = Error
  mov word [packet_data_len], 2
  call send_packet
  jmp _main_end

_handle_s_packet:
  ; Override local EOL marker from sender if available:
  mov byte al, [packet_data_len]
  cmp al, 4
  jb _acknowledge_s_packet
  mov byte bl, [packet_data + 4] ; 'EOL'
  sub bl, 32
  mov [eol_marker], bl

  ; Override local CTL marker from sender if available:
  cmp al, 5
  jb _acknowledge_s_packet
  mov byte bl, [packet_data + 5] ; 'QCTL'
  mov [ctl_marker], bl

_acknowledge_s_packet:
  mov byte [packet_type], 'Y' ; 'Y' = Acknowledgement (ACK)
  mov byte [packet_data],     72  ; 'MAXL'
  mov byte [packet_data + 1], 42  ; 'TIME'
  mov byte [packet_data + 2], 32  ; 'NPAD'
  mov byte [packet_data + 3], 64  ; 'PADC'
  mov byte [packet_data + 4], 45  ; 'EOL' = 0x0d (+ 32)
  mov byte [packet_data + 5], 35  ; 'QCTL' = '#'
  mov byte [packet_data + 6], 'N' ; 'QBIN' = 'N' = Will not do 8-bit quoting.
  mov byte [packet_data + 7], '1' ; 'CHKT' = '1' = Single character checksum.
  mov word [packet_data_len], 8
  call send_packet

_wait_for_file_header_packet:
  ; Get a File Header packet. If a B packet comes, we're all done:
  call get_valid_packet

  mov byte al, [packet_type]
  cmp al, 'B' ; 'B' = Break transmission
  je _handle_b_packet
  cmp al, 'F' ; 'F' = File Header
  je _handle_f_packet

  ; Simplified version of error message: "<?> Packet in F State"
  mov byte [packet_data], al
  mov byte [packet_data + 1], 'F'
  mov byte [packet_type], 'E' ; 'E' = Error
  mov word [packet_data_len], 2
  call send_packet
  jmp _main_end

_handle_b_packet:
  call send_acknowledgement_packet
  jmp _main_end

_handle_f_packet:
  mov dx, message_receiving
  call print_string

  ; Dollar terminate the received filename:
  mov word di, [packet_data_len]
  mov byte [packet_data + di], '$'
  mov dx, packet_data
  call print_string

  ; Zero terminate the received filename:
  mov byte [packet_data + di], 0

  ; Call DOS to create new file and handle.
  mov ah, 0x3c
  mov cx, 0 ; Standard attributes.
  ; DX already containing pointer to packet data.
  int 0x21
  jc _handle_f_packet_error
  mov [file_handle], ax

  call send_acknowledgement_packet

_wait_for_data_packet:
  ; Get Data packets. If a Z packet comes, the file is complete:
  call get_valid_packet

  mov byte al, [packet_type]
  cmp al, 'Z' ; 'Z' = End of file
  je _handle_z_packet
  cmp al, 'D' ; 'D' = Data Packet
  je _handle_d_packet

  ; Simplified version of error message: "<?> Packet in D State"
  mov byte [packet_data], al
  mov byte [packet_data + 1], 'D'
  mov byte [packet_type], 'E' ; 'E' = Error
  mov word [packet_data_len], 2
  call send_packet
  jmp _main_end

_handle_z_packet:
  ; Call DOS to close file handle.
  mov ah, 0x3e
  mov bx, [file_handle]
  int 0x21

  call send_acknowledgement_packet

  mov dx, message_ok
  call print_string

  jmp _wait_for_file_header_packet

_handle_d_packet:
  ; Call DOS to write to file.
  mov ah, 0x40
  mov word bx, [file_handle]
  mov word cx, [packet_data_len]
  mov dx, packet_data
  int 0x21
  jc _handle_d_packet_error

  call send_acknowledgement_packet

  jmp _wait_for_data_packet

_handle_f_packet_error:
  ; Send error code 'C', since new file could not be created.
  mov byte [packet_data], 'C'
  mov byte [packet_type], 'E' ; 'E' = Error
  mov word [packet_data_len], 1
  call send_packet
  jmp _main_end

_handle_d_packet_error:
  ; Send error code 'W', since new file could not be written to.
  mov byte [packet_data], 'W'
  mov byte [packet_type], 'E' ; 'E' = Error
  mov word [packet_data_len], 1
  call send_packet
  jmp _main_end

_main_end:
  mov dx, message_done
  call print_string

  ; Disable interrupt bit on COM1:
  mov dx, COM1_IER
  in al, dx
  and al, 0b11111110 ; Disable Received Data Available Interrupt
  out dx, al

  ; Exit to DOS.
  mov ah, 0x4c
  int 0x21



; PROCEDURE: get_valid_packet
;
; INPUT:
;   N/A
;
; OUTPUT:
;   ds:[packet_type]
;   ds:[packet_data]
;   ds:[packet_data_len]
;
get_valid_packet:
  push ax
  push bx
  push cx

  ; Try to get a valid packet with the desired sequence number.

  mov cx, 5 ; Retry maximum 5 times.
_get_valid_packet_loop:
  call recv_packet

  mov byte al, [recv_seq_no]
  mov byte bl, [send_seq_no]
  cmp al, bl
  jne _get_valid_packet_resend
  mov byte al, [packet_type]
  cmp al, 'Q' ; 'Q' = Block check error
  je _get_valid_packet_resend
  jmp _get_valid_packet_return ; Got a valid packet.

_get_valid_packet_resend:
  call com_port_send ; Contains a retransmit message in send buffer already.

  push dx
  mov dl, '%' ; Indicate retry.
  call print_character
  pop dx

  loop _get_valid_packet_loop ; Until CX = 0

  mov byte [packet_type], 'T' ; 'T' = Timeout

_get_valid_packet_return:
  pop cx
  pop bx
  pop ax
  ret



; PROCEDURE: send_acknowledgement_packet
;
; INPUT:
;   N/A
;
; OUTPUT:
;   N/A
;
send_acknowledgement_packet:
  mov byte [packet_type], 'Y' ; 'Y' = Acknowledgement (ACK)
  mov word [packet_data_len], 0
  call send_packet
  ret



; PROCEDURE: send_packet
;
; INPUT:
;   ds:[packet_type]
;   ds:[packet_data]
;   ds:[packet_data_len]
;   
; OUTPUT:
;   N/A
;
send_packet:
  push ax
  push bx
  push cx
  push di
  push si

  mov di, send_buffer
  mov byte [di], 0x01
  inc di

  mov word ax, [packet_data_len]
  add al, 35 ; 32 + 3 bytes for 'SEQ', 'TYPE' & 'CHECK'
  mov bl, al ; Checksum = LEN
  mov byte [di], al
  inc di

  mov byte al, [send_seq_no]
  add al, 32
  add bl, al ; Checksum += SEQ
  mov byte [di], al
  inc di

  mov byte al, [packet_type]
  add bl, al ; Checksum += TYPE
  mov byte [di], al
  inc di

  mov si, packet_data
  mov word cx, [packet_data_len]
  test cx, cx
  jz _send_packet_empty
_send_packet_loop:
  mov al, [si]
  mov [di], al
  add bl, al ; Checksum += DATA
  inc di
  inc si

  loop _send_packet_loop ; Until CX = 0
_send_packet_empty:

  ; Calculate checksum:
  mov cl, bl
  and cl, 192
  shr cl, 1 ; 8086 is limited to one shift at a time.
  shr cl, 1
  shr cl, 1
  shr cl, 1
  shr cl, 1
  shr cl, 1
  add cl, bl
  and cl, 63
  add cl, 32
  mov byte [di], cl
  inc di

  mov byte al, [eol_marker]
  mov byte [di], al

  mov word ax, [packet_data_len]
  add ax, 6 ; Add 'MARK', 'LEN', 'SEQ', 'TYPE' 'CHECK' and terminator.
  mov word [send_buffer_index], ax

  call com_port_send

  ; Increment packet sequence number:
  inc byte [send_seq_no]
  and byte [send_seq_no], 63 ; Always reduce to 6 bits.

  push dx
  mov dl, '.'; Indicate ACK sent.
  call print_character
  pop dx

  pop si
  pop di
  pop cx
  pop bx
  pop ax
  ret



; PROCEDURE: com_port_send
;
; INPUT:
;   ds:[send_buffer]
;   ds:[send_buffer_index]
;   
; OUTPUT:
;   N/A
;
com_port_send:
  push ax
  push cx
  push dx
  push si

  mov si, 0
  mov word cx, [send_buffer_index]
_com_port_send_byte:

  mov dx, COM1_THR
  mov byte al, [send_buffer + si]
  out dx, al

  mov dx, COM1_LSR
_com_port_send_wait:
  in al, dx
  and al, 0b00100000 ; Empty Transmit Holding Register
  test al, al
  jz _com_port_send_wait ; Busy wait...

  inc si
  loop _com_port_send_byte ; Until CX = 0

  pop si
  pop dx
  pop cx
  pop ax
  ret



; PROCEDURE: recv_packet
;
; INPUT:
;   N/A
;   
; OUTPUT:
;   ds:[packet_type]
;   ds:[packet_data]
;   ds:[packet_data_len]
;   ds:[recv_seq_no]
;
recv_packet:
  push ax
  push bx
  push cx
  push dx
  push di
  push si

  call com_port_recv

  ; Look for 'MARK' start marker 0x01 (Ctrl-A) in buffer:
  mov si, -1
_recv_packet_look_for_marker:
  inc si
  mov ax, [recv_buffer_index]
  cmp si, ax
  jge _recv_packet_fail ; Reached end of buffer.
  mov byte al, [recv_buffer + si]
  cmp al, 0x01 ; (Ctrl-A)
  jne _recv_packet_look_for_marker
  ; Start position now in SI register.

  ; Copy initial packet fields:
  ; * Using AL for data.
  ; * Using BL for checksum.
  ; * Using CX for packet length.
  inc si
  xor cx, cx
  mov byte cl, [recv_buffer + si] ; Packet 'LEN' field.
  mov bl, cl ; Checksum = LEN
  sub cx, 35 ; 32 + 3 bytes for 'SEQ', 'TYPE' & 'CHECK'
  inc si
  mov byte al, [recv_buffer + si] ; Packet 'SEQ' field.
  add bl, al ; Checksum += SEQ
  sub al, 32
  mov byte [recv_seq_no], al
  inc si
  mov byte al, [recv_buffer + si] ; Packet 'TYPE' field.
  add bl, al ; Checksum += TYPE
  mov byte [packet_type], al
  inc si
  ; Packet length now in CX register.
  ; Checksum now in BL register.
  ; Start of data position now in SI register.

  mov word [packet_data_len], cx
  test cx, cx
  jz _recv_packet_checksum ; Zero size packet, skip decoding/copying.

  ; Copy packet data:
  ; * Using AL for data.
  ; * Using BL for checksum.
  ; * Using CX for packet length.
  ; * Using DL for comparisons and temporary storage.
  ; * Using DH for decode flag.
  xor dh, dh
  mov di, packet_data
_recv_packet_copy:
  mov byte al, [recv_buffer + si] ; Packet 'DATA' field.
  add bl, al ; Checksum += DATA
  mov byte dl, [packet_type]
  cmp dl, 'S' ; 'S' = Send Initiation
  jne _recv_packet_decode ; Type 'S' packets shall not be decoded!
  mov [di], al ; Copy un-decoded.
  jmp _recv_packet_increment

_recv_packet_decode:
  cmp dh, 0
  jne _recv_packet_decode_flag_not_set_1
  mov byte dl, [ctl_marker]
  cmp dl, al
  jne _recv_packet_decode_flag_not_set_1
  mov dh, 1
  dec byte [packet_data_len] ; Decoding reduces resulting packet size.
  jmp _recv_packet_increment_source_only

_recv_packet_decode_flag_not_set_1:
  cmp dh, 1
  jne _recv_packet_decode_flag_not_set_2
  xor dh, dh

  mov dl, al
  and dl, 127
  cmp dl, 62
  jng _recv_packet_decode_flag_not_set_2
  cmp dl, 96
  jg _recv_packet_decode_flag_not_set_2
  xor al, 64

_recv_packet_decode_flag_not_set_2:
  mov [di], al
_recv_packet_increment:
  inc di
_recv_packet_increment_source_only:
  inc si
  loop _recv_packet_copy ; Until CX = 0

_recv_packet_checksum:
  ; Calculate and check checksum:
  mov dl, bl
  and dl, 192
  shr dl, 1 ; 8086 is limited to one shift at a time.
  shr dl, 1
  shr dl, 1
  shr dl, 1
  shr dl, 1
  shr dl, 1
  add dl, bl
  and dl, 63
  mov byte al, [recv_buffer + si] ; Packet 'CHECK' field.
  sub al, 32
  cmp al, dl
  jne _recv_packet_fail

  ; All OK!
  jmp _recv_packet_ok

_recv_packet_fail:
  mov byte [packet_type], 'Q' ; 'Q' = Block check error
_recv_packet_ok:
  pop si
  pop di
  pop dx
  pop cx
  pop bx
  pop ax
  ret



; PROCEDURE: com_port_recv
;
; INPUT:
;   N/A
;   
; OUTPUT:
;   ds:[recv_buffer]
;   ds:[recv_buffer_index]
;
com_port_recv:
  push ax
  push dx
  push di

  mov di, 0
_com_port_recv_byte:

  mov dx, COM1_IIR
_com_port_recv_wait:
  in al, dx
  and al, 0b00001110 ; Identification
  cmp al, 0b00000100 ; Enable Received Data Available Interrupt
  jne _com_port_recv_wait ; Busy wait...

  mov dx, COM1_RBR
  in al, dx

  cmp di, RECV_BUFFER_SIZE
  jb _com_port_recv_copy
  mov di, 0 ; Reset and wrap to prevent overflow.
_com_port_recv_copy:
  mov byte [recv_buffer + di], al
  inc di

  ; Keep reading until a terminator 0x0d (Carriage Return) arrives:
  cmp al, 0x0d
  jne _com_port_recv_byte

  mov word [recv_buffer_index], di

  pop di
  pop dx
  pop ax
  ret



; PROCEDURE: print_character
;
; INPUT:
;   dl
;   
; OUTPUT:
;   N/A
;
print_character:
  push ax
  push dx
  ; Call DOS to display character:
  mov ah, 0x2
  ; DL set by caller...
  int 0x21
  pop dx
  pop ax
  ret



; PROCEDURE: print_string
;
; INPUT:
;   dx
;   
; OUTPUT:
;   N/A
;
print_string:
  push ax
  ; Call DOS to display string:
  mov ah, 0x9
  ; DS is already same as CS, no need to change.
  ; DX set by caller...
  int 0x21
  pop ax
  ret



section .data:
recv_buffer_index:
  dw 0 ; 16-bit word due to SI/DI registers.
recv_buffer:
  times RECV_BUFFER_SIZE db 0

send_buffer_index:
  dw 0 ; 16-bit word due to SI/DI registers.
send_buffer:
  times SEND_BUFFER_SIZE db 0

send_seq_no:
  db 0
recv_seq_no:
  db 0

packet_type:
  db 0
packet_data_len:
  dw 0 ; 16-bit word due to SI/DI registers.
packet_data:
  times PACKET_DATA_SIZE db 0

ctl_marker:
  db '#'  ; Default
eol_marker:
  db 0x0d ; Default

file_handle:
  dw 0

message_waiting:
  db "Waiting...", 0x0d, 0x0a, "$"
message_receiving:
  db "Receiving: $"
message_ok:
  db "(OK)", 0x0d, 0x0a, "$"
message_done:
  db "(DONE)", 0x0d, 0x0a, "$"



