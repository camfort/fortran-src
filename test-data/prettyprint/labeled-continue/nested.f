      PROGRAM NESTED
      INTEGER I,J, SUM
      SUM = 0

      DO 100 I = 1,5
        DO 100 J = 1,5
            SUM = SUM + 1
100   CONTINUE

      PRINT *, 'SUM = ', SUM
      END