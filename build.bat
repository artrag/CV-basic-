:: tmscolor -z -t -b -o result.bmp ScrollTest.bmp ScrollTest.bas

..\cvbasic --msx  ScrollTest.bas output.asm
..\gasm80 output.asm -o output.rom
