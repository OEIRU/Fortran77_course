      PROGRAM main
      INTEGER n_segments
      PARAMETER (n_segments = 10)
      REAL a, b
      EXTERNAL fun
      a = 0.0
      b = 1.0

      ! Верификация для метода трапеций
      CALL verification_table('Trapezoid', 2, a, b, n_segments)

      ! Верификация для метода Гаусса-4
      CALL verification_table('Gauss-4', 8, a, b, n_segments)
      END

      ! Подпрограмма для создания сетки
      SUBROUTINE create_grid(a, b, n, x)
      REAL a, b, x(n)
      INTEGER i, n
      REAL h
      h = (b - a) / (n - 1)
      DO i = 1, n
          x(i) = a + (i-1)*h
      END DO
      END

      ! Метод трапеций
      SUBROUTINE trapezoid_rule(x, n, integral, degree)
      REAL x(n), integral, h
      INTEGER i, n, degree
      integral = 0.0
      DO i = 1, n-1
          h = x(i+1) - x(i)
          integral = integral + h * (fun(x(i), degree) + 
     &                              fun(x(i+1), degree)) / 2.0
      END DO
      END

      ! Метод Гаусса-4
      SUBROUTINE gauss4_rule(x, n, integral, degree)
      REAL x(n), integral, h, mid, a, b
      REAL w(4), xi(4)
      INTEGER i, j, n, degree
      DATA w /0.3478548451, 0.6521451549, 0.6521451549, 0.3478548451/
      DATA xi /-0.8611363116, -0.3399810436, 0.3399810436, 0.8611363116/
      integral = 0.0
      DO i = 1, n-1
          a = x(i)
          b = x(i+1)
          mid = (a + b) / 2.0
          h = (b - a) / 2.0
          DO j = 1, 4
              integral = integral + w(j) * fun(mid + h * xi(j), degree) * h
          END DO
      END DO
      END

      ! Функция для вычисления аналитического значения интеграла
      REAL FUNCTION analytic_integral(a, b, degree)
      REAL a, b
      INTEGER degree
      analytic_integral = (b**(degree+1) - a**(degree+1)) / (degree + 1)
      END

      ! Подпрограмма для верификации
      SUBROUTINE verification_table(method_name, k, a, b, n_segments)
      CHARACTER*(*) method_name
      INTEGER k, n_segments
      REAL a, b
      REAL x(1000), integral, integral_2h, analytic, error, error_2h
      REAL ratio, runge_error, richardson, richardson_error
      INTEGER degree, m
      m = k  ! Порядок метода

      PRINT *, 'Method: ', method_name
      PRINT *, 'Order of method: ', k
      PRINT *, '-------------------------------------------'

      DO degree = m-1, m+2
          analytic = analytic_integral(a, b, degree)

          ! Вычисление для N отрезков
          CALL create_grid(a, b, n_segments, x)
          IF (method_name .EQ. 'Trapezoid') THEN
              CALL trapezoid_rule(x, n_segments, integral, degree)
          ELSE IF (method_name .EQ. 'Gauss-4') THEN
              CALL gauss4_rule(x, n_segments, integral, degree)
          END IF
          error = ABS(analytic - integral)

          ! Вычисление для 2N отрезков
          CALL create_grid(a, b, n_segments*2, x)
          IF (method_name .EQ. 'Trapezoid') THEN
              CALL trapezoid_rule(x, n_segments*2, integral_2h, degree)
          ELSE IF (method_name .EQ. 'Gauss-4') THEN
              CALL gauss4_rule(x, n_segments*2, integral_2h, degree)
          END IF
          error_2h = ABS(analytic - integral_2h)

          ! Отношение погрешностей
          IF (error .NE. 0.0) THEN
              ratio = error_2h / error
          ELSE
              ratio = 0.0
          END IF

          ! Оценка погрешности по правилу Рунге
          runge_error = error_2h / (2**k - 1)

          ! Уточнение по Ричардсону
          richardson = integral + (integral - integral_2h) / (2**k - 1)
          richardson_error = ABS(analytic - richardson)

          ! Вывод данных построчно
          PRINT *, 'Degree: ', degree
          PRINT *, 'Analytic: ', analytic
          PRINT *, 'N: ', n_segments
          PRINT *, 'Numeric: ', integral
          PRINT *, 'Error: ', error
          PRINT *, 'Ratio: ', ratio
          PRINT *, 'Runge Error: ', runge_error
          PRINT *, 'Richardson: ', richardson
          PRINT *, 'Richardson Error: ', richardson_error
          PRINT *, '-------------------------------------------'
      END DO
      END

      ! Интегрируемая функция
      REAL FUNCTION fun(x, degree)
      REAL x
      INTEGER degree
      IF (degree .EQ. 1) THEN
          fun = x
      ELSE IF (degree .EQ. 2) THEN
          fun = x**2
      ELSE IF (degree .EQ. 3) THEN
          fun = x**3
      ELSE IF (degree .EQ. 4) THEN
          fun = x**4
      ELSE
          fun = 0.0
      END IF
      END