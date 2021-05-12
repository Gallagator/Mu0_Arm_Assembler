

    .label
JMP $4095
JMI $128
JEQ $128
JMP .label
JEQ .another_label



.another_label

  STP
  STP
  STP
  STP
  STP
  STP
  STP

    ADD R0, $12 ROR $8
         SUB R3, R2 ASR $15 


TST R3, R3 ROR $15

LDR R2, [R3]

    LDR R3, [R3, $15]!
