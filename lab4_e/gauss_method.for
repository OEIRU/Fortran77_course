      PROGRAM GAUSS_QUADRATURE
      IMPLICIT NONE
      REAL*8 A, B, ANALYTIC_VAL, CURRENT_INTEGRAL
      REAL*8 PREV_INTEGRAL, RUNGE_ERROR, RICHARDSON_CORRECTION
      REAL*8 ERROR, PREV_ERROR, RUNGE_RATIO, CORRECTED_ERROR
      INTEGER MAX_STEPS, N, TIMER_COUNT
      LOGICAL TIMER_ACTIVE
      EXTERNAL F  ! Явное объявление функции F
      REAL*8 ANALYTIC_VALUE  ! Явное объявление типа функции ANALYTIC_VALUE
      REAL*8 GAUSS_QUADRATURE_METHOD  ! Явное объявление типа функции GAUSS_QUADRATURE_METHOD
      REAL*8 GAUSS_SUM  ! Явное объявление типа функции GAUSS_SUM

      A = 0.0D0
      B = 1.0D0
      MAX_STEPS = 25
      ANALYTIC_VAL = ANALYTIC_VALUE(A, B)

C Вывод заголовков таблицы
      PRINT *, 'Number of segments Step Numerical value'
      PRINT *, 'Error ratio Error'
      PRINT *, 'Runge estimate Richardson correction'
      PRINT *, 'Error of corrected solution'

      PREV_INTEGRAL = 0.0D0
      PREV_ERROR = 0.0D0
      TIMER_ACTIVE = .FALSE.  ! Инициализация флага таймера
      TIMER_COUNT = 0         ! Счётчик для таймера

      DO N = 0, MAX_STEPS
         IF (.NOT. TIMER_ACTIVE .AND. N > 0) THEN
            IF (ERROR > PREV_ERROR) THEN
               TIMER_ACTIVE = .TRUE.
               TIMER_COUNT = 3
               PRINT *, 'Timer activated for 3 iterations!'
            END IF
         END IF

         CALL CREATE_GRID(A, B, 2**N, CURRENT_INTEGRAL)
         ERROR = ABS(CURRENT_INTEGRAL - ANALYTIC_VAL)

         IF (N .GT. 0) THEN
            RUNGE_RATIO = ERROR / PREV_ERROR
            RUNGE_ERROR = ABS(CURRENT_INTEGRAL - PREV_INTEGRAL) / 15.0D0
            RICHARDSON_CORRECTION = CURRENT_INTEGRAL + RUNGE_ERROR
         ELSE
            RUNGE_RATIO = 0.0D0
            RUNGE_ERROR = 0.0D0
            RICHARDSON_CORRECTION = CURRENT_INTEGRAL
         END IF

         CORRECTED_ERROR = ABS(RICHARDSON_CORRECTION - ANALYTIC_VAL)

C Форматированный вывод результатов
         PRINT 100, 2**N, (B-A)/(2**N), CURRENT_INTEGRAL,
     &         RUNGE_RATIO, ERROR, RUNGE_ERROR,
     &         RICHARDSON_CORRECTION, CORRECTED_ERROR
  100 FORMAT (I8, F10.6, E15.8, E15.8, E15.8, E15.8, E15.8, E15.8)

         PREV_INTEGRAL = CURRENT_INTEGRAL
         PREV_ERROR = ERROR

C Обработка таймера
         IF (TIMER_ACTIVE) THEN
            TIMER_COUNT = TIMER_COUNT - 1
            IF (TIMER_COUNT <= 0) THEN
               PRINT *, 'Timer expired. Stopping the program.'
               GO TO 1000  ! Выход из программы
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

      SUBROUTINE CREATE_GRID(A, B, N, RESULT)
      IMPLICIT NONE
      REAL*8 A, B, RESULT
      INTEGER N
      EXTERNAL F
      REAL*8 GAUSS_QUADRATURE_METHOD  ! Явное объявление типа функции GAUSS_QUADRATURE_METHOD
      RESULT = GAUSS_QUADRATURE_METHOD(F, A, B, N)
      END

      REAL*8 FUNCTION GAUSS_QUADRATURE_METHOD(FUNC, A, B, N)
      IMPLICIT NONE
      REAL*8 FUNC, A, B, H, SUM
      INTEGER N, I
      EXTERNAL FUNC
      REAL*8 X(4), W(4)
      REAL*8 GAUSS_SUM  ! Явное объявление типа функции GAUSS_SUM

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
         SUM = SUM + H/2.0D0 * GAUSS_SUM(FUNC, A, B, 
     &    (2.0D0 * DBLE(I - 1) + 1.0D0) * H / 2.0D0 - 1.0D0, H, X, W)
      END DO

      GAUSS_QUADRATURE_METHOD = SUM
      END

      REAL*8 FUNCTION GAUSS_SUM(FUNC, A, B, T, H, X, W)
      IMPLICIT NONE
      REAL*8 FUNC, A, B, T, H
      REAL*8 X(4), W(4)
      INTEGER I
      EXTERNAL FUNC
      REAL*8 VALUE
      VALUE = 0.0D0
      DO I = 1, 4
         VALUE = VALUE + W(I) * FUNC((B-A)/2.0D0*T + (B+A)/2.0D0)
      END DO
      GAUSS_SUM = VALUE
      END

      REAL*8 FUNCTION ANALYTIC_VALUE(A, B)
      IMPLICIT NONE
      REAL*8 A, B
      ANALYTIC_VALUE = (-DCOS(10.0D0 * B) / 10.0D0) +
     &                 (DCOS(10.0D0 * A) / 10.0D0)
      END