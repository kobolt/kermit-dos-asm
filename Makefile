
kermit.com: kermit.asm
	nasm kermit.asm -fbin -o kermit.com

.PHONY: clean
clean:
	rm -f kermit.com

