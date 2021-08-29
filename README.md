# 6303
Items related to HD6303 MCU

These files are mainly for my HD6303 board and its various components.

Included are drivers and test routines for:<br>
AY-3-89xx PSG<br>
YMZ284 PSG<br>
SN76489 PSG<br>
HD6321 PIO<br>
R6551 serial interface<br>
LCD interface for 2 or 4 line by x character displays<br>

Note files:<br>
format is [note][octave],[duration]<br>
for pause use P0,[duration]<br>
Notes sequence including half notes:<br>
C   d   D   e   E   F   g   G   a   A   b   B<br>
C  C#   D  D#   E   F  F#   G  G#   A  A#   B<br>
<br>
To convert notes file to asm include use following command in Linux shell:<br>
<pre>cat LightMyFire.note | tr -d '\r' | ./sn76notes2db.bash > sn76light.asm</pre><br>
<br>
To compile using dasm use following command:<br>
<pre>dasm 6303test.asm -f3 -l6303test.lst -o6303test.bin</pre><br>

