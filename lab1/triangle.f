      PROGRAM MAIN
      ! Объявление общих блоков для обмена данными между подпрограммами.
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c
      COMMON /shared/ S, pi

      REAL minAngle, minCos
      pi = 3.14159265
      DO WHILE (i .NE. 5)
      CALL menu()      ! Вызов подпрограммы для вывода меню
      CALL input(i)    ! Вызов подпрограммы для вывода выбора пользователя
      SELECT CASE (i)  ! Определение операций switch
      
      CASE (1)
      CALL setData()
      CALL calculateGamma()
      IF (a.GT.0 .AND. b.GT.0 .AND. alpha.LT.180) THEN
c      IF (a.LT.b+c .AND. b.LT.a+c .AND. c.LT.a+b) THEN
      CALL calculateS()
      ELSE
      PRINT *, 'Данная фигура не может быть треугольником.'
      END IF

      CASE (2)
      CALL output('S = ', S)
      
      CASE (3)
      CALL output('Минимальный угол = ', minAngle())
      
      CASE (4)
      CALL output('Min cos = ', minCos())
      CASE ( : 0, 6 : )
      PRINT *, 'Неверный ввод.'
      END SELECT
      PRINT *, ' '
      END DO
      
      END

      SUBROUTINE menu()
      PRINT *, 'Выберите пункт:'
      PRINT *, '1. Ввод нового треугольника.'
      PRINT *, '2. Вычислить площадь треугольника.'
      PRINT *, '3. Минимальный угол в градусах.'
      PRINT *, '4. Косинус минимального угла.'
      PRINT *, '5. Выход.'
      END

      SUBROUTINE input(item)
      INTEGER item
      PRINT *, ' '
      PRINT *, 'Выберите пункт:'
      READ *, item
      END

      SUBROUTINE output(text, result)
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c
      COMMON /shared/ S, pi
      CHARACTER *(*) text
      REAL result
c      IF (S .NE. 0) THEN
      PRINT *, text, result
c      ELSE
c      PRINT *, 'Данная фигура не может быть треугольником.'
c      END IF
      END

      SUBROUTINE setData() ! Ввод данных 
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c 
      COMMON /shared/ S, pi
      PRINT *, 'Введите данные:'
      PRINT *, 'Сторона a: '
      READ *, a
      PRINT *, 'Сторона b: '
      READ *, b
      PRINT *, 'Угол между ними: '
      READ *, alpha
      END

      SUBROUTINE calculateGamma() ! Вычисление углов
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c 
      COMMON /shared/ S, pi
      c = sqrt(a**2+b**2-2*a*b*cos(alpha * pi / 180))
      beta = acos((b**2 + c**2 - a**2)/(2*b*c)) * 180 / pi  
      gamma = 180 - alpha - beta 
      PRINT *, 'Значение переменной a:', a 
      PRINT *, 'Значение переменной b:', b 
      PRINT *, 'Значение переменной с:', c 
      PRINT *, 'Значение переменной alpha:', alpha
      PRINT *, 'Значение переменной beta:', beta
      PRINT *, 'Значение переменной gamma:', gamma
      END
 
      SUBROUTINE calculateS() ! Вычисление площади 
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c 
      COMMON /shared/ S, pi
      S = 0.5 * a * b * sin(alpha * pi / 180)
      END

      REAL FUNCTION minAngle() ! Поиск минимального угла
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c 
      COMMON /shared/ S, pi
      minAngle = min(alpha, beta, gamma) 
      END

      REAL FUNCTION minCos() ! Поиск минимального косинуса
      COMMON /angles/ alpha, beta, gamma
      COMMON /side/ a, b, c
      COMMON /shared/ S, pi
      minCos = cos(min(alpha, beta, gamma) * pi / 180)
      END

