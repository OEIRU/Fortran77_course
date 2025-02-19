      PROGRAM INTEGRATION
      IMPLICIT NONE
      REAL*8 A, B, ANALYTIC_VAL, CURRENT_INTEGRAL
      REAL*8 PREV_INTEGRAL, RUNGE_ERROR, RICHARDSON_CORRECTION
      REAL*8 ERROR, PREV_ERROR, RUNGE_RATIO, CORRECTED_ERROR, H
      INTEGER MAX_STEPS, N, NUM_SEGMENTS

      EXTERNAL F  ! Явное объявление функции F
      REAL*8 ANALYTIC_VALUE  ! Явное объявление типа функции ANALYTIC_VALUE
      REAL*8 TRAPEZOIDAL_METHOD  ! Явное объявление типа функции TRAPEZOIDAL_METHOD

      A = 0.0D0
      B = 1.0D0
      MAX_STEPS = 15
      ANALYTIC_VAL = ANALYTIC_VALUE(A, B)

C Вывод заголовков таблицы
      PRINT *, 'Number of segments Step Numerical value'
      PRINT *, 'Error ratio Error'
      PRINT *, 'Runge estimate Richardson correction'
      PRINT *, 'Error of corrected solution'

      PREV_INTEGRAL = 0.0D0
      PREV_ERROR = 0.0D0

      DO N = 0, MAX_STEPSANALYTIC_VALUE
         NUM_SEGMENTS = 2**N
         CALL CREATE_GRID(A, B, NUM_SEGMENTS, H)
         CURRENT_INTEGRAL = TRAPEZOIDAL_METHOD(F, A, B, NUM_SEGMENTS, H)
         ERROR = ABS(CURRENT_INTEGRAL - ANALYTIC_VAL)

         IF (N .GT. 0) THEN
            RUNGE_RATIO = ERROR / PREV_ERROR
            RUNGE_ERROR = ABS(CURRENT_INTEGRAL - PREV_INTEGRAL) / 3.0D0
            RICHARDSON_CORRECTION = CURRENT_INTEGRAL + RUNGE_ERROR
         ELSE
            RUNGE_RATIO = 0.0D0
            RUNGE_ERROR = 0.0D0
            RICHARDSON_CORRECTION = CURRENT_INTEGRAL
         END IF

         CORRECTED_ERROR = ABS(RICHARDSON_CORRECTION - ANALYTIC_VAL)

C Форматированный вывод результатов
         PRINT 100, NUM_SEGMENTS, H, CURRENT_INTEGRAL,
     &         RUNGE_RATIO, ERROR, RUNGE_ERROR,
     &         RICHARDSON_CORRECTION, CORRECTED_ERROR
  100 FORMAT (I6, F10.6, E15.8, E15.8, E15.8, E15.8, E15.8, E15.8)

         PREV_INTEGRAL = CURRENT_INTEGRAL
         PREV_ERROR = ERROR
      END DO

      END

      REAL*8 FUNCTION F(X)
      IMPLICIT NONE
      REAL*8 X
      F = DSIN(10.0D0 * X)
      !F = X**5
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

      REAL*8 FUNCTION ANALYTIC_VALUE(A, B)
      IMPLICIT NONE
      REAL*8 A, B
      ANALYTIC_VALUE = (-DCOS(10.0D0 * B) / 10.0D0) +
     &                 (DCOS(10.0D0 * A) / 10.0D0)
      END