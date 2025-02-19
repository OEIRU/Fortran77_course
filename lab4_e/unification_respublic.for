      PROGRAM INTEGRATION
      IMPLICIT NONE
      REAL*8 A, B, ANALYTIC_VAL, CURRENT_INTEGRAL
      REAL*8 PREV_INTEGRAL, RUNGE_ERROR, RICHARDSON_CORRECTION
      REAL*8 ERROR, PREV_ERROR, RUNGE_RATIO, CORRECTED_ERROR, H
      INTEGER MAX_STEPS, N, NUM_SEGMENTS, METHOD
      LOGICAL TIMER_ACTIVE
      INTEGER TIMER_COUNT
      EXTERNAL F
      REAL*8 ANALYTIC_VALUE
      REAL*8 TRAPEZOIDAL_METHOD
      REAL*8 GAUSS_QUADRATURE_METHOD

C Выбор метода интегрирования: 1 - Метод трапеций, 2 - Квадратура Гаусса
      PRINT *, 'Choose integration method: 1 for Trapez, 2 for Gauss'
      READ *, METHOD
      A = 0.0D0
      B = 1.0D0
      MAX_STEPS = 25
      ANALYTIC_VAL = ANALYTIC_VALUE(A, B)
      PRINT *, 'Number of segments Step Numerical value'
      PRINT *, 'Error ratio Error'
      PRINT *, 'Runge estimate Richardson correction'
      PRINT *, 'Error of corrected solution'
      PREV_INTEGRAL = 0.0D0
      PREV_ERROR = 0.0D0
      TIMER_ACTIVE = .FALSE.
      TIMER_COUNT = 0
      DO N = 0, MAX_STEPS
        IF (.NOT. TIMER_ACTIVE .AND. N > 0) THEN
          IF (ERROR > PREV_ERROR) THEN
            TIMER_ACTIVE = .TRUE.
            TIMER_COUNT = 3
            PRINT *, 'Timer activated for 3 iterations!'
          END IF
        END IF
        NUM_SEGMENTS = 2**N
        CALL CREATE_GRID(A, B, NUM_SEGMENTS, H)
        IF (METHOD .EQ. 1) THEN
        CURRENT_INTEGRAL = TRAPEZOIDAL_METHOD(F, A, B, NUM_SEGMENTS, H)
        ELSE IF (METHOD .EQ. 2) THEN
       CURRENT_INTEGRAL = GAUSS_QUADRATURE_METHOD(F, A, B, NUM_SEGMENTS)
        ELSE
          PRINT *, 'Invalid method selected. Using Trapezoidal method.'
        CURRENT_INTEGRAL = TRAPEZOIDAL_METHOD(F, A, B, NUM_SEGMENTS, H)
        END IF
        ERROR = ABS(CURRENT_INTEGRAL - ANALYTIC_VAL)
        IF (N .GT. 0) THEN
          RUNGE_RATIO = ERROR / PREV_ERROR
          IF (METHOD .EQ. 1) THEN
            RUNGE_ERROR = ABS(CURRENT_INTEGRAL - PREV_INTEGRAL) / 3.0D0
          ELSE IF (METHOD .EQ. 2) THEN
            RUNGE_ERROR = ABS(CURRENT_INTEGRAL - PREV_INTEGRAL) / 15.0D0
          END IF
          RICHARDSON_CORRECTION = CURRENT_INTEGRAL + RUNGE_ERROR
        ELSE
          RUNGE_RATIO = 0.0D0
          RUNGE_ERROR = 0.0D0
          RICHARDSON_CORRECTION = CURRENT_INTEGRAL
        END IF
        CORRECTED_ERROR = ABS(RICHARDSON_CORRECTION - ANALYTIC_VAL)
        PRINT 100, NUM_SEGMENTS, H, CURRENT_INTEGRAL,
     &         RUNGE_RATIO, ERROR, RUNGE_ERROR,
     &         RICHARDSON_CORRECTION, CORRECTED_ERROR
  100 FORMAT (I8, F10.6, E15.8, E15.8, E15.8, E15.8, E15.8, E15.8)
        PREV_INTEGRAL = CURRENT_INTEGRAL
        PREV_ERROR = ERROR
        IF (TIMER_ACTIVE) THEN
          TIMER_COUNT = TIMER_COUNT - 1
          IF (TIMER_COUNT <= 0) THEN
            PRINT *, 'Timer expired. Stopping the program.'
            GO TO 1000
          END IF
        END IF
      END DO
 1000 CONTINUE
      END

      REAL*8 FUNCTION F(X)
      IMPLICIT NONE
      REAL*8 X
      F = DSIN(10.0D0 * X)
      END

      SUBROUTINE CREATE_GRID(A, B, N, H)
      IMPLICIT NONE
      REAL*8 A, B, H
      INTEGER N
      H = (B - A) / DBLE(N)
      END

      REAL*8 FUNCTION TRAPEZOIDAL_METHOD(FUNC, A, B, N, H)
      IMPLICIT NONE
      REAL*8 FUNC, A, B, H, RESULT, X
      INTEGER N, I
      EXTERNAL FUNC
      RESULT = 0.5D0 * (FUNC(A) + FUNC(B))
      DO I = 1, N-1
        X = A + DBLE(I) * H
        RESULT = RESULT + FUNC(X)
      END DO
      TRAPEZOIDAL_METHOD = RESULT * H
      END

      REAL*8 FUNCTION GAUSS_QUADRATURE_METHOD(FUNC, A, B, N)
      IMPLICIT NONE
      REAL*8 FUNC, A, B, H, SUM
      INTEGER N, I
      EXTERNAL FUNC
      REAL*8 X(4), W(4)
      REAL*8 GAUSS_SUM
      X(1) = -0.8611363116D0
      X(2) = -0.3399810436D0
      X(3) = 0.3399810436D0
      X(4) = 0.8611363116D0
      W(1) = 0.3478547486D0
      W(2) = 0.6521451549D0
      W(3) = 0.6521451549D0
      W(4) = 0.3478547486D0
      H = (B - A) / DBLE(N)
      SUM = 0.0D0
      DO I = 1, N
        SUM = SUM + GAUSS_SUM(FUNC, A + (DBLE(I)-1.0D0)*H,
     &                        A + DBLE(I)*H, X, W)
      END DO
      GAUSS_QUADRATURE_METHOD = SUM
      END

      REAL*8 FUNCTION GAUSS_SUM(FUNC, A, B, X, W)
      IMPLICIT NONE
      REAL*8 FUNC, A, B
      REAL*8 X(4), W(4)
      INTEGER I
      EXTERNAL FUNC
      REAL*8 VALUE, XI
      VALUE = 0.0D0
      DO I = 1, 4
        XI = (B-A)/2.0D0*X(I) + (A+B)/2.0D0
        VALUE = VALUE + W(I) * FUNC(XI)
      END DO
      GAUSS_SUM = (B-A)/2.0D0 * VALUE
      END

      REAL*8 FUNCTION ANALYTIC_VALUE(A, B)
      IMPLICIT NONE
      REAL*8 A, B
      ANALYTIC_VALUE = (-DCOS(10.0D0 * B) / 10.0D0) +
     &                 (DCOS(10.0D0 * A) / 10.0D0)
      END