MOV R1, $1 ROR $0
MOV R2, $10 ROR $0

.loop
ADD R1, $1 ROR $0
CMP R1, R2 ROR $0
JMI .loop


