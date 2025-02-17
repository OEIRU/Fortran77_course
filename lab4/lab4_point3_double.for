      PROGRAM MAIN
      REAL*8 A, B
      INTEGER MAX_ITER, FUNC_TYPE
      PARAMETER (MAX_ITER = 100)
      CHARACTER*20 METHOD_NAME
      INTEGER METHOD

      PRINT *, 'Enter A:'
      READ *, A
      PRINT *, 'Enter B:'
      READ *, B

      DO METHOD = 1, 2
         IF (METHOD .EQ. 1) THEN
            METHOD_NAME = 'Trapezoid'
         ELSE
            METHOD_NAME = 'Gauss-4'
         END IF

         DO FUNC_TYPE = 1, 2
            CALL CONVERGENCE_TABLE(METHOD_NAME, METHOD, A, B, FUNC_TYPE)
         END DO
      END DO

      END

      SUBROUTINE BUILD_GRID(A, B, N, X)
      REAL*8 A, B, X(*)
      INTEGER N, I

      DO I = 1, N
         X(I) = A + (B - A) * (I - 1) / (N - 1)
      END DO

      END

      REAL*8 FUNCTION TRAPEZOID_INTEGRAL(X, N, FUN)
      INTEGER N
      REAL*8 X(N), FUN
      EXTERNAL FUN
      INTEGER I
      REAL*8 H

      H = X(2) - X(1)
      TRAPEZOID_INTEGRAL = (FUN(X(1)) + FUN(X(N))) / 2.0D0
      DO I = 2, N-1
         TRAPEZOID_INTEGRAL = TRAPEZOID_INTEGRAL + FUN(X(I))
      END DO
      TRAPEZOID_INTEGRAL = TRAPEZOID_INTEGRAL * H
      END

      REAL*8 FUNCTION GAUSS4_INTEGRAL(X, N, FUN)
      INTEGER N
      REAL*8 X(N), FUN
      EXTERNAL FUN
      INTEGER I, J
      REAL*8 W(4), XI(4)

      DATA W /0.3478548451D0, 0.6521451549D0, 
     &       0.6521451549D0, 0.3478548451D0/
      DATA XI /-0.8611363116D0, -0.3399810436D0, 
     &       0.3399810436D0, 0.8611363116D0/

      GAUSS4_INTEGRAL = 0.0D0
      DO I = 1, N-1
         H = X(I+1) - X(I)
         MID = (X(I) + X(I+1)) / 2.0D0
         DO J = 1, 4
            T_J = MID + (H / 2.0D0) * XI(J)
            GAUSS4_INTEGRAL = GAUSS4_INTEGRAL + W(J) * FUN(T_J)
         END DO
         GAUSS4_INTEGRAL = GAUSS4_INTEGRAL * (H / 2.0D0)
      END DO
      END

      REAL*8 FUNCTION INTEGRATE(METHOD, X, N, FUN)
      INTEGER METHOD, N
      REAL*8 X(N), FUN
      EXTERNAL FUN

      IF (METHOD .EQ. 1) THEN
         INTEGRATE = TRAPEZOID_INTEGRAL(X, N, FUN)
      ELSE IF (METHOD .EQ. 2) THEN
         INTEGRATE = GAUSS4_INTEGRAL(X, N, FUN)
      END IF
      END

      REAL*8 FUNCTION ANALYTIC_INTEGRAL(A, B, FUNC_TYPE)
      REAL*8 A, B
      INTEGER FUNC_TYPE

      IF (FUNC_TYPE .EQ. 1) THEN
         ANALYTIC_INTEGRAL = (B**6 - A**6) / 6.0D0
      ELSE IF (FUNC_TYPE .EQ. 2) THEN
         ANALYTIC_INTEGRAL = (-COS(10.0D0 * B) + COS(10.0D0 * A)) / 10.0D0
      END IF
      END

      REAL*8 FUNCTION FUN(X, FUNC_TYPE)
      REAL*8 X
      INTEGER FUNC_TYPE

      IF (FUNC_TYPE .EQ. 1) THEN
         FUN = X**5
      ELSE IF (FUNC_TYPE .EQ. 2) THEN
         FUN = SIN(10.0D0 * X)
      END IF
      END

      SUBROUTINE CALCULATE_ERRORS(ANALYTIC, INTEGRAL, PREV_INTEGRAL, K,
     &                            ERROR, RATIO_ERROR, RUNGE_ERROR,
     &                            RICHARDSON, RICHARDSON_ERROR)
      REAL*8 ANALYTIC, INTEGRAL, PREV_INTEGRAL, ERROR, RATIO_ERROR,
     &     RUNGE_ERROR, RICHARDSON, RICHARDSON_ERROR
      INTEGER K

      ERROR = ABS(ANALYTIC - INTEGRAL)
      RATIO_ERROR = 0.0D0
      IF (PREV_INTEGRAL .NE. 0.0D0 .AND. ERROR .GT. 1.0D-15) THEN
         RATIO_ERROR = ABS((PREV_INTEGRAL - ANALYTIC) / ERROR)
      END IF

      RUNGE_ERROR = 0.0D0
      IF (PREV_INTEGRAL .NE. 0.0D0) THEN
         RUNGE_ERROR = (INTEGRAL - PREV_INTEGRAL) / (2**K - 1)
      END IF

      RICHARDSON = INTEGRAL + RUNGE_ERROR
      RICHARDSON_ERROR = ABS(ANALYTIC - RICHARDSON)
      END

      SUBROUTINE CONVERGENCE_TABLE(METHOD_NAME, METHOD, A, B, FUNC_TYPE)
      CHARACTER*(*) METHOD_NAME
      INTEGER METHOD, FUNC_TYPE
      REAL*8 A, B, ANALYTIC, INTEGRAL, ERROR, PREV_ERROR, MIN_ERROR
      REAL*8 RUNGE_ERROR, RICHARDSON, RICHARDSON_ERROR
      REAL*8 PREV_INTEGRAL, RATIO_ERROR
      INTEGER N_SEGMENTS, STEP, ITER_AFTER_MIN
      LOGICAL FLAG_MIN
      INTEGER MAX_ITER
      PARAMETER (MAX_ITER = 100)

      REAL*8 X(1000000)  ! Массив для сетки

      REAL*8 ANALYTIC_INTEGRAL
      REAL*8 TRAPEZOID_INTEGRAL
      REAL*8 GAUSS4_INTEGRAL
      REAL*8 INTEGRATE
      REAL*8 FUN
      EXTERNAL FUN

      ANALYTIC = ANALYTIC_INTEGRAL(A, B, FUNC_TYPE)
      WRITE(*, *) 'Method: ', METHOD_NAME
      WRITE(*, *) 'Function: ', FUNC_TYPE
      WRITE(*, *) 'Analytic: ', ANALYTIC

      PREV_ERROR = 1.0D30
      PREV_INTEGRAL = 0.0D0
      FLAG_MIN = .FALSE.
      ITER_AFTER_MIN = 0
      MIN_ERROR = 1.0D30
      STEP = 1
      N_SEGMENTS = 1

      DO WHILE (N_SEGMENTS .LE. 1E9 .AND. STEP .LE. MAX_ITER)
         CALL BUILD_GRID(A, B, N_SEGMENTS, X)

         INTEGRAL = INTEGRATE(METHOD, X, N_SEGMENTS, FUN)
         CALL CALCULATE_ERRORS(ANALYTIC, INTEGRAL, PREV_INTEGRAL, METHOD,
     &                         ERROR, RATIO_ERROR, RUNGE_ERROR,
     &                         RICHARDSON, RICHARDSON_ERROR)

         WRITE(*, 100) 'N: ', N_SEGMENTS
         WRITE(*, 101) 'Error: ', ERROR
         WRITE(*, 101) 'Ratio Error: ', RATIO_ERROR
         WRITE(*, 101) 'Richardson Error: ', RICHARDSON_ERROR

         IF (.NOT. FLAG_MIN) THEN
            IF (ERROR .LT. MIN_ERROR) THEN
               MIN_ERROR = ERROR
            ELSE IF (ERROR .GT. MIN_ERROR) THEN
               FLAG_MIN = .TRUE.
            END IF
         END IF

         IF (FLAG_MIN) THEN
            ITER_AFTER_MIN = ITER_AFTER_MIN + 1
            IF (ITER_AFTER_MIN .GE. 3) THEN
               EXIT
            END IF
         END IF

         PREV_ERROR = ERROR
         PREV_INTEGRAL = INTEGRAL
         N_SEGMENTS = N_SEGMENTS * 2
         STEP = STEP + 1
      END DO

100   FORMAT(A, I6)
101   FORMAT(A, E20.12)

      END

! ON HO
! THIS CRINGE DONT WORK
! COMMIT THIS
! GOING SLEEP 
