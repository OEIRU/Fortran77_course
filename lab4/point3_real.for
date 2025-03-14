      PROGRAM INTEGRATION
      REAL A, B, ANALYTIC_VAL, CURRENT_INTEGRAL
      REAL PREV_INTEGRAL, RUNGE_ERROR, RICHARDSON_CORRECTION
      REAL ERROR, PREV_ERROR, RUNGE_RATIO, CORRECTED_ERROR, H
      INTEGER MAX_STEPS, N, NUM_SEGMENTS, METHOD, COUNTER
      EXTERNAL F
      REAL ANALYTIC_VALUE
      REAL TRAPEZOIDAL_METHOD
      REAL GAUSS_METHOD

      PRINT *, 'Choose integration method: 1 for Trapez, 2 for Gauss'
      READ *, METHOD
      A = 0.0 
      B = 100.0 
      MAX_STEPS = 20 
      ANALYTIC_VAL = ANALYTIC_VALUE(A, B)
      PRINT *, 'Number of segments \\ Numerical value'
      PRINT *, 'Error'
      PRINT *, 'Runge estimate \\ Richardson correction'
      PRINT *, 'Error of corrected solution'

      PREV_INTEGRAL = 0.0 
      PREV_ERROR = 1000
      COUNTER = 0

      DO N = 0, MAX_STEPS
       NUM_SEGMENTS = 2**N
       CALL CREATE_GRID(A, B, NUM_SEGMENTS, H)

       IF (METHOD .EQ. 1) THEN
       CURRENT_INTEGRAL = TRAPEZOIDAL_METHOD(F, A, B, NUM_SEGMENTS, H)
       ELSE IF (METHOD .EQ. 2) THEN
       CURRENT_INTEGRAL = GAUSS_METHOD(F, A, B, NUM_SEGMENTS, H)
       ELSE
       PRINT *, 'Invalid method selected. Using Trapezoidal method.'
       CURRENT_INTEGRAL = TRAPEZOIDAL_METHOD(F, A, B, NUM_SEGMENTS, H)
       END IF

       ERROR = ABS(ANALYTIC_VAL - CURRENT_INTEGRAL)
       IF (N .GT. 0) THEN
         RUNGE_RATIO = ERROR / PREV_ERROR
         IF (METHOD .EQ. 1) THEN
          RUNGE_ERROR = ABS(CURRENT_INTEGRAL - PREV_INTEGRAL) / 3.0 
          ELSE IF (METHOD .EQ. 2) THEN
          RUNGE_ERROR = ABS(CURRENT_INTEGRAL - PREV_INTEGRAL) / 255.0 
         END IF
         RICHARDSON_CORRECTION = CURRENT_INTEGRAL + RUNGE_ERROR
         ELSE
          RUNGE_RATIO = 0.0 
          RUNGE_ERROR = 0.0 
          RICHARDSON_CORRECTION = CURRENT_INTEGRAL
         END IF

         CORRECTED_ERROR = ABS(RICHARDSON_CORRECTION - ANALYTIC_VAL)

    1 FORMAT(I10, E15.8, E15.8, E15.8, E15.8, E15.8, E15.8)
         PRINT 1, NUM_SEGMENTS, CURRENT_INTEGRAL,
     &        ERROR, RUNGE_ERROR,
     &        RICHARDSON_CORRECTION, CORRECTED_ERROR

         PREV_INTEGRAL = CURRENT_INTEGRAL
         PREV_ERROR = ERROR
      END DO

 1000 CONTINUE
      END

      REAL FUNCTION F(X)
      REAL X
      !F = X**5
       F = SIN(10*X)
      END

      REAL FUNCTION ANALYTIC_VALUE(A, B)
      REAL A, B
      !ANALYTIC_VALUE = (B**6 - A**6) / 6.0

      ANALYTIC_VALUE = (-DCOS(10.0D0 * B) / 10.0D0) +
     &                 (DCOS(10.0D0 * A) / 10.0D0)
      END

      SUBROUTINE CREATE_GRID(A, B, N, H)
      REAL A, B, H
      INTEGER N
      H = (B - A) / REAL(N)
      END

      REAL FUNCTION TRAPEZOIDAL_METHOD(FUNC, A, B, N, H)
      REAL FUNC, A, B, H, RESULT, X
      INTEGER N, I
      EXTERNAL FUNC
      RESULT = 0.5 * (FUNC(A) + FUNC(B))
      DO I = 1, N-1
       X = A + REAL(I) * H
       RESULT = RESULT + FUNC(X)
      END DO
      TRAPEZOIDAL_METHOD = RESULT * H
      END

      REAL FUNCTION GAUSS_METHOD(FUNC, A, B, N, H)
      REAL FUNC, A, B, H, SUM, GAUSS_SUM
      INTEGER N, I
      EXTERNAL FUNC
      REAL X(4), W(4)
      DATA X /-0.8611363, -0.3399810,
     &  0.3399810, 0.8611363/
      DATA W /0.3478547, 0.6521451,
     &  0.6521451, 0.3478547/

      !H = (B - A) / REAL(N)
      SUM = 0.0 
      DO I = 1, N
       SUM = SUM + GAUSS_SUM(FUNC, A + 
     & (REAL(I)-1.0 )*H, A + REAL(I)*H, X, W)
      END DO
      GAUSS_METHOD = SUM
      END

      REAL FUNCTION GAUSS_SUM(FUNC, A, B, X, W)
      REAL FUNC, A, B
      REAL X(4), W(4)
      INTEGER I
      EXTERNAL FUNC
      REAL VALUE, XI, HALF_WIDTH, MID_POINT
      HALF_WIDTH = (B - A) / 2.0 
      MID_POINT = (A + B) / 2.0 
      VALUE = 0.0 
      DO I = 1, 4
       XI = HALF_WIDTH * X(I) + MID_POINT
       VALUE = VALUE + W(I) * FUNC(XI)
      END DO
      GAUSS_SUM = HALF_WIDTH * VALUE
      END
