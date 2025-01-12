      PROGRAM main
      REAL*8 a, b, integral
      EXTERNAL fun, fun_wrapper
      a = 0.0D0
      b = 1.0D0

      ! Верификация для метода трапеций
      CALL verification_table('Trapezoid', 2, a, b)

      ! Верификация для метода Гаусса-4
      CALL verification_table('Gauss-4', 8, a, b)
      END

      ! Подпрограмма для верификации методов
      SUBROUTINE verification_table(method_name, k, a, b)
      CHARACTER*(*) method_name
      INTEGER k
      REAL*8 a, b
      REAL*8 integral, integral_2h, analytic, error, error_2h
      REAL*8 ratio, runge_error, richardson, richardson_error
      INTEGER degree, n_current
      INTEGER degrees(7), n_values(7)
      INTEGER i
      EXTERNAL trapezoid_rule, gauss4_rule, fun_wrapper
      REAL*8 analytic_integral  ! Явное объявление функции как REAL*8

      ! Массивы для хранения комбинаций degree и N
      DATA degrees /1, 2, 2, 3, 3, 4, 4/
      DATA n_values /1, 1, 2, 1, 2, 1, 2/

      PRINT *, 'Method: ', method_name
      PRINT *, 'Order of method: ', k
      PRINT *, '-------------------------------------------'

      ! Перебор всех комбинаций degree и N
      DO i = 1, 7
          degree = degrees(i)
          n_current = n_values(i)

          analytic = analytic_integral(a, b, degree)  ! Аналитическое значение

          ! Вычисление интеграла для N отрезков
          IF (method_name .EQ. 'Trapezoid') THEN
            CALL trapezoid_rule(a, b, n_current, integral, degree)
          ELSE IF (method_name .EQ. 'Gauss-4') THEN
            CALL gauss4_rule(a, b, n_current, integral, degree)
          END IF
          error = ABS(analytic - integral)  ! Погрешность

          ! Вычисление интеграла для 2N отрезков
          IF (method_name .EQ. 'Trapezoid') THEN
            CALL trapezoid_rule(a, b, n_current*2, integral_2h, degree)
          ELSE IF (method_name .EQ. 'Gauss-4') THEN
            CALL gauss4_rule(a, b, n_current*2, integral_2h, degree)
          END IF
          error_2h = ABS(analytic - integral_2h)  ! Погрешность для 2N

          ! Отношение погрешностей
          IF (error .NE. 0.0D0) THEN
              ratio = error_2h / error
          ELSE
              ratio = 0.0D0
          END IF

          ! Оценка погрешности по правилу Рунге
          runge_error = error_2h / (2**k - 1)

          ! Уточнение по Ричардсону
          richardson = integral + (integral - integral_2h) / (2**k - 1)
          richardson_error = ABS(analytic - richardson)

          ! Вывод результатов
          PRINT *, 'Degree: ', degree
          PRINT *, 'N: ', n_current
          PRINT *, 'Analytic: ', analytic
          PRINT *, 'Numeric: ', integral
          PRINT *, 'Error: ', error
          PRINT *, 'Ratio: ', ratio
          PRINT *, 'Runge Error: ', runge_error
          PRINT *, 'Richardson: ', richardson
          PRINT *, 'Richardson Error: ', richardson_error
          PRINT *, '-------------------------------------------'
      END DO
      END

      ! Метод трапеций
      SUBROUTINE trapezoid_rule(a, b, n, integral, degree)
      REAL*8 a, b, integral, h, x
      INTEGER i, n, degree
      REAL*8 fun_wrapper
      EXTERNAL fun_wrapper

      integral = 0.0D0
      h = (b - a) / n
      DO i = 0, n-1
          x = a + i * h
          integral = integral + (fun_wrapper(x, degree) + 
     &                          fun_wrapper(x + h, degree)) * h / 2.0D0
      END DO
      END

      ! Метод Гаусса-4
      SUBROUTINE gauss4_rule(a, b, n, integral, degree)
      REAL*8 a, b, integral, h, a_i, b_i, mid, t_j
      REAL*8 w(4), xi(4)
      INTEGER i, j, n, degree
      REAL*8 fun_wrapper
      EXTERNAL fun_wrapper

      ! Веса и узлы для метода Гаусса-4
      DATA w /0.3478548451D0, 0.6521451549D0, 
     &        0.6521451549D0, 0.3478548451D0/
      DATA xi /-0.8611363116D0, -0.3399810436D0, 
     &         0.3399810436D0, 0.8611363116D0/

      integral = 0.0D0
      h = (b - a) / n  ! Шаг разбиения

      ! Цикл по подынтервалам
      DO i = 0, n-1
        a_i = a + i * h  ! Начало подынтервала
        b_i = a_i + h    ! Конец подынтервала
        mid = (a_i + b_i) / 2.0D0  ! Середина подынтервала

        ! Цикл по узлам Гаусса-4
        DO j = 1, 4
            t_j = mid + (h / 2.0D0) * xi(j)  ! Масштабирование узла
            integral = integral + w(j) * 
     &                  fun_wrapper(t_j, degree) * (h / 2.0D0)
        END DO
      END DO
      END

      ! Функция для вычисления аналитического значения интеграла
      REAL*8 FUNCTION analytic_integral(a, b, degree)
      REAL*8 a, b
      INTEGER degree
      analytic_integral = (b**(degree+1) - a**(degree+1)) / 
     &                    (degree + 1)
      END

      ! Обертка для функции fun
      REAL*8 FUNCTION fun_wrapper(x, degree)
      REAL*8 x
      INTEGER degree
      REAL*8 fun
      EXTERNAL fun
      fun_wrapper = fun(x, degree)
      END

      ! Интегрируемая функция (полином степени degree)
      REAL*8 FUNCTION fun(x, degree)
      REAL*8 x
      INTEGER degree
      IF (degree .EQ. 1) THEN
          fun = x
      ELSE IF (degree .EQ. 2) THEN
          fun = x**2
      ELSE IF (degree .EQ. 3) THEN
          fun = x**3
      ELSE IF (degree .EQ. 4) THEN
          fun = x**4
      ELSE IF (degree .EQ. 5) THEN
          fun = x**5
      ELSE IF (degree .EQ. 6) THEN
          fun = x**6
      ELSE
          fun = 0.0D0  ! По умолчанию
      END IF
      END