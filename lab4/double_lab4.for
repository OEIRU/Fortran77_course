      PROGRAM MAIN
      IMPLICIT NONE
      DOUBLE PRECISION H, A, B
      INTEGER N, MAX, I
      COMMON /GRIDPARAM/ H, A, B, N
      DOUBLE PRECISION GRID(1000000)

      PRINT *, '1) TRAPEZIA'
      PRINT *, '3) GAUSS-4'
      PRINT *, '4) VIHOD'
      
      READ *, I
      IF (I .EQ. 4) GOTO 100

      PRINT *, 'ENTER SEGMENT AND [A,B]'
      READ *, MAX, A, B

      N = 1  ! Начинаем с 2^0 = 1
      IF (I .EQ. 1) THEN
          CALL TRAPEZE(GRID, MAX)
      ELSE IF (I .EQ. 3) THEN
          CALL GAUSS4(GRID, MAX)
      ELSE
          PRINT *, 'INVALID OPTION'
      END IF

  100 END

      SUBROUTINE GRIDMAKE(GRID)
      IMPLICIT NONE
      DOUBLE PRECISION H, A, B
      INTEGER N, I
      COMMON /GRIDPARAM/ H, A, B, N
      DOUBLE PRECISION GRID(N)

      H = (B - A) / DBLE(N - 1)
      GRID(1) = A
      DO 10 I = 2, N
          GRID(I) = A + H * DBLE(I - 1)
   10 CONTINUE
      END

      SUBROUTINE TRAPEZE(GRID, MAX)
      IMPLICIT NONE
      DOUBLE PRECISION H, A, B
      INTEGER N, MAX, I
      COMMON /GRIDPARAM/ H, A, B, N
      DOUBLE PRECISION GRID(N)
      DOUBLE PRECISION ANS, FUN
      EXTERNAL FUN

   20 IF (N .LE. MAX) THEN
          CALL GRIDMAKE(GRID)
          ANS = H * (FUN(GRID(1)) + FUN(GRID(N))) / 2.0D0
          DO 30 I = 2, N - 1
              ANS = ANS + H * FUN(GRID(I))
   30     CONTINUE
          PRINT *, 'TRAPEZOIDAL RULE RESULT FOR N =', N, ':', ANS
          N = 2 * N  ! Удваиваем N
          GOTO 20
      END IF
      END

      SUBROUTINE GAUSS4(GRID, MAX)
      IMPLICIT NONE
      DOUBLE PRECISION H, A, B
      INTEGER N, MAX, I
      COMMON /GRIDPARAM/ H, A, B, N
      DOUBLE PRECISION GRID(N)
      DOUBLE PRECISION ANS, X1, X2, X3, X4, W1, W2, W3, W4, MID, FUN
      EXTERNAL FUN

      W1 = 0.3478548451D0
      W2 = 0.6521451549D0
      W3 = 0.6521451549D0
      W4 = 0.3478548451D0
      X1 = -0.8611363116D0
      X2 = -0.3399810436D0
      X3 = 0.3399810436D0
      X4 = 0.8611363116D0

   40 IF (N .LE. MAX) THEN
          CALL GRIDMAKE(GRID)
          ANS = 0.0D0
          DO 50 I = 1, N - 1
              H = (GRID(I + 1) - GRID(I)) / 2.0D0
              MID = (GRID(I + 1) + GRID(I)) / 2.0D0
              ANS = ANS + H * (W1 * FUN(MID + H * X1) + 
     &                         W2 * FUN(MID + H * X2) +
     &                         W3 * FUN(MID + H * X3) + 
     &                         W4 * FUN(MID + H * X4))
   50     CONTINUE
          PRINT *, 'GAUSS-4 RULE RESULT FOR N =', N, ':', ANS
          N = 2 * N  ! Удваиваем N
          GOTO 40
      END IF
      END

      DOUBLE PRECISION FUNCTION FUN(X)
      IMPLICIT NONE
      DOUBLE PRECISION X
      FUN = 1000.0D0 * SIN(5.0D0 * X)
      END