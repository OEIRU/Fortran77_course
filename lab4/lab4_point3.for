      PROGRAM MAIN
      INTEGER N_SEGMENTS
      REAL A, B
      EXTERNAL POLY_FUNC, OSC_FUNC
      REAL POLY_FUNC, OSC_FUNC
      CHARACTER*20 METHOD_NAME, FUNC_NAME
  
      ! Ввод значений A, B
      PRINT *, 'Enter A:'
      READ *, A
      PRINT *, 'Enter B:'
      READ *, B
  
      ! Исследование для метода трапеций
      METHOD_NAME = 'Trapezoid'
      FUNC_NAME = 'Poly k+3'
      N_SEGMENTS = 1
      CALL CONVERGENCE_TABLE(METHOD_NAME, 2, A, B, N_SEGMENTS, 
     &                     POLY_FUNC, FUNC_NAME)
  
      FUNC_NAME = 'Oscillating'
      N_SEGMENTS = 1
      CALL CONVERGENCE_TABLE(METHOD_NAME, 2, A, B, N_SEGMENTS, 
     &                    OSC_FUNC, FUNC_NAME)
  
      ! Исследование для метода Гаусса-4
      METHOD_NAME = 'Gauss-4'
      FUNC_NAME = 'Poly k+3'
      N_SEGMENTS = 1
      CALL CONVERGENCE_TABLE(METHOD_NAME, 8, A, B, N_SEGMENTS, 
     &                     POLY_FUNC, FUNC_NAME)
  
      FUNC_NAME = 'Oscillating'
      N_SEGMENTS = 1
      CALL CONVERGENCE_TABLE(METHOD_NAME, 8, A, B, N_SEGMENTS, 
     &                     OSC_FUNC, FUNC_NAME)
      END
  
      ! Метод трапеций
      SUBROUTINE TRAPEZOID_RULE(A, B, N, INTEGRAL, FUNC)
      REAL A, B, INTEGRAL, H, X
      INTEGER I, N
      REAL FUNC
      EXTERNAL FUNC
  
      INTEGRAL = 0.0 
      H = (B - A) / N
      DO I = 0, N-1
        X = A + I * H
        INTEGRAL = INTEGRAL + (FUNC(X) + FUNC(X + H)) * H / 2.0 
      END DO
      END
  
      ! Метод Гаусса-4
      SUBROUTINE GAUSS4_RULE(A, B, N, INTEGRAL, FUNC)
      REAL A, B, INTEGRAL, H, A_I, B_I, MID, T_J
      REAL W(4), XI(4)
      INTEGER I, J, N
      REAL FUNC
      EXTERNAL FUNC
  
      ! Веса и узлы для метода Гаусса-4
      DATA W /0.34785484, 0.65214515, 
     &      0.65214515, 0.34785484/
      DATA XI /-0.8611363116, -0.3399810436, 
     &      0.3399810436, 0.8611363116/
  
      INTEGRAL = 0.0 
      H = (B - A) / N
  
      DO I = 0, N-1
        A_I = A + I * H
        B_I = A_I + H
        MID = (A_I + B_I) / 2.0
  
        DO J = 1, 4
          T_J = MID + (H / 2.0) * XI(J)
          INTEGRAL = INTEGRAL + W(J) * FUNC(T_J) * (H / 2.0)
        END DO
      END DO
      END
  
      ! Функция для вычисления аналитического значения интеграла
      REAL FUNCTION ANALYTIC_INTEGRAL(A, B, FUNC_NAME)
      REAL A, B
      CHARACTER*(*) FUNC_NAME
      INTEGER IS_POLY, IS_OSC
  
      IS_POLY = 0
      IS_OSC = 0
      IF (FUNC_NAME .EQ. 'Poly k+3') IS_POLY = 1
      IF (FUNC_NAME .EQ. 'Oscillating') IS_OSC = 1
  
      IF (IS_POLY .EQ. 0 .AND. IS_OSC .EQ. 0) THEN
        PRINT *, 'Error: Unknown function name.'
        ANALYTIC_INTEGRAL = 0.0 
        RETURN
      END IF
  
      ANALYTIC_INTEGRAL = IS_POLY * (B**6 - A**6) / 6.0 
     &                + IS_OSC * (-COS(10.0 * B) + COS(10.0 * A)) 
     &                / 10.0 
      END
  
      ! Подпрограмма для исследования порядка аппроксимации
      SUBROUTINE CONVERGENCE_TABLE(METHOD_NAME, K, A, B, N_SEGMENTS, 
     &                          FUNC, FUNC_NAME)
      CHARACTER*(*) METHOD_NAME, FUNC_NAME
      INTEGER K, N_SEGMENTS
      REAL A, B
      REAL FUNC
      EXTERNAL FUNC
      REAL INTEGRAL, ANALYTIC, ERROR, PREV_ERROR, MIN_ERROR
      REAL RUNGE_ERROR, RICHARDSON, RICHARDSON_ERROR
      REAL PREV_INTEGRAL, RATIO_ERROR, EST_RATIO_ERROR
      INTEGER STEP, ITER_AFTER_MIN
      INTEGER IS_TRAPEZOID, IS_GAUSS
      LOGICAL FLAG_MIN
      REAL ANALYTIC_INTEGRAL
      INTEGER MAX_ITER
      PARAMETER (MAX_ITER = 1000)
        
      IS_TRAPEZOID = 0
      IS_GAUSS = 0
      IF (METHOD_NAME .EQ. 'Trapezoid') IS_TRAPEZOID = 1
      IF (METHOD_NAME .EQ. 'Gauss-4') IS_GAUSS = 1
  
      ANALYTIC = ANALYTIC_INTEGRAL(A, B, FUNC_NAME)
      WRITE(*, *) 'Method: ', METHOD_NAME
      WRITE(*, *) 'Order of method: ', K
      WRITE(*, *) 'Function: ', FUNC_NAME
      WRITE(*, *) 'Analytic: ', ANALYTIC
      WRITE(*, *) '-------------------------------------------'
  
      PREV_ERROR = 1.0E30
      PREV_INTEGRAL = 0.0 
      FLAG_MIN = .FALSE.
      ITER_AFTER_MIN = 0
      MIN_ERROR = 1.0E30
  
      STEP = 1
      DO WHILE (N_SEGMENTS .LE. 1E6 .AND. STEP .LE. MAX_ITER)  ! Ограничение N
        IF (IS_TRAPEZOID .EQ. 1) THEN
          CALL TRAPEZOID_RULE(A, B, N_SEGMENTS, INTEGRAL, FUNC)
        ELSE IF (IS_GAUSS .EQ. 1) THEN
          CALL GAUSS4_RULE(A, B, N_SEGMENTS, INTEGRAL, FUNC)
        END IF
        ERROR = ABS(ANALYTIC - INTEGRAL)
  
        IF (STEP .GT. 1 .AND. ERROR .GT. 1.0E-7) THEN  ! Условие для выхода
          RATIO_ERROR = PREV_ERROR / ERROR
          EST_RATIO_ERROR = 2.0 **K
        END IF
  
        WRITE(*, 100) 'N: ', N_SEGMENTS
        WRITE(*, 101) 'Error: ', ERROR
  
        IF (.NOT. FLAG_MIN .AND. ERROR .LT. MIN_ERROR) THEN
          MIN_ERROR = ERROR
        END IF
  
        IF (.NOT. FLAG_MIN .AND. ERROR .GT. MIN_ERROR) THEN
          FLAG_MIN = .TRUE.
        END IF
  
        IF (FLAG_MIN) THEN
          ITER_AFTER_MIN = ITER_AFTER_MIN + 1
        END IF
  
        IF (ITER_AFTER_MIN .GE. 3) THEN 
          EXIT
        END IF
  
        PREV_ERROR = ERROR
        PREV_INTEGRAL = INTEGRAL
        N_SEGMENTS = N_SEGMENTS * 2
        STEP = STEP + 1
      END DO
100   FORMAT(A, I60)
101   FORMAT(A, E15.7)
      END
  
      ! Полином степени k+3 (для k=2: x^5)
      REAL FUNCTION POLY_FUNC(X)
      REAL X
      POLY_FUNC = X**5 
      END
  
      ! Осциллирующая функция (sin(10x))
      REAL FUNCTION OSC_FUNC(X)
      REAL X
      OSC_FUNC = SIN(10.0 * X)
      END