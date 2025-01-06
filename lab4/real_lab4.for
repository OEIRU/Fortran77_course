    PROGRAM LAB4
        REAL A, B, INTEGRAL_TRAPEZE, INTEGRAL_GAUSS
        INTEGER N, DEGREE
        PARAMETER (NMAX = 1000)  ! Максимальное количество узлов сетки
        REAL GRID(NMAX)          ! Одномерный массив для сетки
        EXTERNAL FUN             ! Интегрируемая функция

        ! Ввод данных с клавиатуры
        PRINT *, 'Enter lower limit (A):'
        READ *, A
        PRINT *, 'Enter upper limit (B):'
        READ *, B
        PRINT *, 'Enter number of grid points (N):'
        READ *, N
        PRINT *, 'Enter polynomial degree'
        PRINT *, '(1, 2, 3 for Trapeze; 3, 4, 5 for Gauss-4):'
        READ *, DEGREE

        ! Проверка на корректность введенных данных
        IF (N .GT. NMAX) THEN
            PRINT *, 'Error: N exceeds maximum allowed value (', NMAX, ')'
            STOP
        END IF
        IF (A .GE. B) THEN
            PRINT *, 'Error: A must be less than B'
            STOP
        END IF

        ! Построение сетки
        CALL BUILD_GRID(A, B, N, GRID)

        ! Вычисление аналитического значения интеграла
        CALL ANALYTICAL_INTEGRAL(A, B, DEGREE, ANALYTICAL_VALUE)

        ! Вычисление интеграла методом трапеций
        CALL TRAPEZE_METHOD(GRID, N, INTEGRAL_TRAPEZE)
        CALL PRINT_TABLE('Trapeze', 2, DEGREE, A, B, ANALYTICAL_VALUE,
    &                 N, INTEGRAL_TRAPEZE)

        ! Вычисление интеграла методом Гаусса-4
        CALL GAUSS4_METHOD(GRID, N, INTEGRAL_GAUSS)
        CALL PRINT_TABLE('Gauss-4', 4, DEGREE, A, B, ANALYTICAL_VALUE,
    &                 N, INTEGRAL_GAUSS)

        END

        ! Интегрируемая функция (полином)
        REAL FUNCTION FUN(X, DEGREE)
        REAL X
        INTEGER DEGREE
        IF (DEGREE .EQ. 1) THEN
            FUN = X
        ELSE IF (DEGREE .EQ. 2) THEN
            FUN = X**2
        ELSE IF (DEGREE .EQ. 3) THEN
            FUN = X**3
        ELSE IF (DEGREE .EQ. 4) THEN
            FUN = X**4
        ELSE IF (DEGREE .EQ. 5) THEN
            FUN = X**5
        ELSE
            FUN = 0.0
        END IF
        RETURN
        END

        ! Подпрограмма для построения равномерной сетки
        SUBROUTINE BUILD_GRID(A, B, N, GRID)
        REAL A, B, GRID(N)
        INTEGER N, I
        REAL H
        H = (B - A) / (N - 1)
        DO I = 1, N
            GRID(I) = A + (I - 1) * H
        END DO
        RETURN
        END

        ! Подпрограмма для метода трапеций
        SUBROUTINE TRAPEZE_METHOD(GRID, N, INTEGRAL)
        REAL GRID(N), INTEGRAL
        INTEGER N, I
        REAL H, SUM
        H = GRID(2) - GRID(1)
        SUM = 0.0
        DO I = 2, N - 1
            SUM = SUM + FUN(GRID(I), DEGREE)
        END DO
        INTEGRAL = H * (0.5 * (FUN(GRID(1), DEGREE) +
    & FUN(GRID(N), DEGREE)) + SUM)
        RETURN
        END

        ! Подпрограмма для метода Гаусса-4
        SUBROUTINE GAUSS4_METHOD(GRID, N, INTEGRAL)
        REAL GRID(N), INTEGRAL
        INTEGER N, I
        REAL H, X1, X2, X3, X4, W1, W2, W3, W4
        PARAMETER (W1 = 0.3478548451, W2 = 0.6521451549,
    &           W3 = 0.6521451549, W4 = 0.3478548451)
        PARAMETER (X1 = -0.8611363116, X2 = -0.3399810436,
    &           X3 = 0.3399810436, X4 = 0.8611363116)
        H = GRID(2) - GRID(1)
        INTEGRAL = 0.0
        DO I = 1, N - 1
            INTEGRAL = INTEGRAL + H / 2.0 * (
    &        W1 * FUN((GRID(I+1) - GRID(I)) / 2.0 * X1 +
    &                 (GRID(I+1) + GRID(I)) / 2.0, DEGREE) +
    &        W2 * FUN((GRID(I+1) - GRID(I)) / 2.0 * X2 +
    &                 (GRID(I+1) + GRID(I)) / 2.0, DEGREE) +
    &        W3 * FUN((GRID(I+1) - GRID(I)) / 2.0 * X3 +
    &                 (GRID(I+1) + GRID(I)) / 2.0, DEGREE) +
    &        W4 * FUN((GRID(I+1) - GRID(I)) / 2.0 * X4 +
    &                 (GRID(I+1) + GRID(I)) / 2.0, DEGREE))
        END DO
        RETURN
        END

        ! Подпрограмма для вычисления аналитического значения интеграла
        SUBROUTINE ANALYTICAL_INTEGRAL(A, B, DEGREE, ANALYTICAL_VALUE)
        REAL A, B, ANALYTICAL_VALUE
        INTEGER DEGREE
        IF (DEGREE .EQ. 1) THEN
            ANALYTICAL_VALUE = (B**2 - A**2) / 2.0
        ELSE IF (DEGREE .EQ. 2) THEN
            ANALYTICAL_VALUE = (B**3 - A**3) / 3.0
        ELSE IF (DEGREE .EQ. 3) THEN
            ANALYTICAL_VALUE = (B**4 - A**4) / 4.0
        ELSE IF (DEGREE .EQ. 4) THEN
            ANALYTICAL_VALUE = (B**5 - A**5) / 5.0
        ELSE IF (DEGREE .EQ. 5) THEN
            ANALYTICAL_VALUE = (B**6 - A**6) / 6.0
        ELSE
            ANALYTICAL_VALUE = 0.0
        END IF
        RETURN
        END

        ! Подпрограмма для вывода таблицы
        SUBROUTINE PRINT_TABLE(METHOD_NAME, K, DEGREE, A, B,
    &                       ANALYTICAL_VALUE, N, NUMERICAL_VALUE)
        CHARACTER*(*) METHOD_NAME
        INTEGER K, DEGREE, N
        REAL A, B, ANALYTICAL_VALUE, NUMERICAL_VALUE
        REAL ERROR, RUNGE_ERROR, RICHARDSON_VALUE, RICHARDSON_ERROR

        ! Вычисление погрешности
        ERROR = ABS(ANALYTICAL_VALUE - NUMERICAL_VALUE)

        ! Оценка погрешности по правилу Рунге
        IF (N .GT. 1) THEN
            RUNGE_ERROR = ERROR / (2**K - 1)
        ELSE
            RUNGE_ERROR = 0.0
        END IF

        ! Уточнение по Ридчарсону
        IF (N .GT. 1) THEN
            RICHARDSON_VALUE = NUMERICAL_VALUE + RUNGE_ERROR
            RICHARDSON_ERROR = ABS(ANALYTICAL_VALUE - RICHARDSON_VALUE)
        ELSE
            RICHARDSON_VALUE = 0.0
            RICHARDSON_ERROR = 0.0
        END IF

        ! Вывод таблицы
        PRINT *, '------------------------------------------------------'
        PRINT *, 'Method: ', METHOD_NAME
        PRINT *, 'Polynomial degree: ', DEGREE
        PRINT *, 'Interval: [', A, ', ', B, ']'
        PRINT *, 'Analytical value: ', ANALYTICAL_VALUE
        PRINT *, 'Number of segments: ', N
        PRINT *, 'Numerical value: ', NUMERICAL_VALUE
        PRINT *, 'Error: ', ERROR
        PRINT *, 'Runge error estimate: ', RUNGE_ERROR
        PRINT *, 'Richardson value: ', RICHARDSON_VALUE
        PRINT *, 'Richardson error: ', RICHARDSON_ERROR
        PRINT *, '------------------------------------------------------'

        RETURN
        END