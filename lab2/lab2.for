      PROGRAM MAIN                                        
          IMPLICIT NONE
          COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP  ! Глобальные переменные
          REAL X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP            
          
          CALL READ_PARAMS()                                         ! Вызов функций
          
          CALL CREATE_TABLE()
          
      END


      SUBROUTINE READ_PARAMS()
          IMPLICIT NONE
          COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          REAL X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          
          OPEN(10, FILE='params.txt', ERR=100)            ! Открытие файла
          READ(10, *, ERR=101) X_MIN, X_MAX
          READ(10, *, ERR=101) Y_MIN, Y_MAX
          READ(10, *, ERR=101) X_STEP, Y_STEP
          CLOSE(10)
          
          IF(X_MAX.LE.X_MIN.OR.Y_MAX.LE.Y_MIN) THEN       ! Если начальное больше конечного - ошибка 
              GOTO 102
          ENDIF
          
          IF(X_STEP.LE.0.0.OR.Y_STEP.LE.0.0) THEN         ! шаг <= 0 - ошибка
              GOTO 103
          ENDIF
          
          GOTO 109
          
100       PAUSE 'Open params file error!'
          STOP
          
101       PAUSE 'Read params file error!'
          CLOSE(10)
          STOP
          
102       PAUSE 'Incorrect data, max must be greater than min!'
          STOP
          
103       PAUSE 'Incorrect data, step must be greater than 0!'
          STOP    
          
109       CONTINUE              
      END


      REAL FUNCTION TO_RAD(ANGLE)                    ! Функция перевод градус в радиан
          IMPLICIT NONE
          REAL ANGLE
          TO_RAD = ANGLE / 180 * 3.1415926
      END


      INTEGER FUNCTION IS_INVISIBLE(FIRST, SECOND)   ! Проверка на невидимый шаг
          IMPLICIT NONE
          REAL FIRST, SECOND
          CHARACTER*11 STR1, STR2 ! создание переменных
          IS_INVISIBLE = 0        ! Тумблер
          
          WRITE(STR1, '(E11.4)') FIRST
          WRITE(STR2, '(E11.4)') SECOND
          
          IF(STR1.EQ.STR2) THEN
              IS_INVISIBLE = 1                       ! числа одинаковые - true
          ENDIF
          
          RETURN
      END



      REAL FUNCTION ARCCOS(X, Y)
          IMPLICIT NONE
          REAL X, Y, TO_RAD, SUM

          SUM = X + Y
             
          IF (SUM.LT. -1.0.OR. SUM.GT. 1.0) THEN      ! Условие
            ARCCOS = 0.0
            RETURN
          ELSE
            ARCCOS = ACOS(TO_RAD(X + Y))    !  Арко (x + y)
        ENDIF
          
      END


      
      SUBROUTINE WRITE_ROW(X, M, REAL_M)       ! Логика таблицы (x_min, ?, real?)
          IMPLICIT NONE
          COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP          
          REAL X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          REAL X, Y, ARCCOS_RES
          REAL ARCCOS
          INTEGER M, I, J, IS_INVISIBLE, REAL_M
          
          WRITE(10, 11, ERR=101) '|'           ! Запись заголовка таблицы 
          WRITE(10, 10, ERR=101) X, '|'        ! Запись переменной х в заголовок
                                               ! Запись первого столбца 
          Y = Y_MIN                            ! Создание переменной для обхода по Y
          ARCCOS_RES = ARCCOS(X, Y)              ! подсчет первой итеации
          IF(ARCCOS_RES.NE.0.0) THEN
              WRITE(10, 10, ERR=101) ARCCOS_RES, '|'    ! Если результат не 0 - вывод
          ELSE
              WRITE(10, 11, ERR=101) '        N/D|'    ! Результат 0 - N/D
          ENDIF
          
          DO J=1, M-2, 1                       ! Запись решений с 2 до M-1 столбца 
              IF(Y + Y_STEP.GT.Y_MAX) THEN
                  EXIT                         ! Y > Y_max - выход
              ENDIF  
              IF(IS_INVISIBLE(Y, Y + Y_STEP).EQ.1) THEN
                  Y = Y + Y_STEP               ! Работа с невидимым шагом
                  CYCLE                    
              ENDIF
              Y = Y + Y_STEP                   ! Следующий Y 
              IF(ABS(Y).LT.ABS(Y_STEP/2)) THEN
                  Y = 0                        ! Если Y около нуля - занулить (??)
              ENDIF
              ARCCOS_RES = ARCCOS(X, Y)          ! Само вычисление

              IF(ARCCOS_RES.NE.0.0) THEN
                  WRITE(10, 10, ERR=101) ARCCOS_RES, '|' ! Запись 
              ELSE
                  WRITE(10, 11, ERR=101) '        N/D|' ! Вывод N/D
              ENDIF
          ENDDO
          
          ! Запись последнего столбца
          IF(IS_INVISIBLE(Y, Y_MAX).EQ.0) THEN   
            ARCCOS_RES = ARCCOS(X, Y_MAX)      ! Если точно считается - считаем 
              IF(ARCCOS_RES.NE.0.0) THEN
                  WRITE(10, 10, ERR=101) ARCCOS_RES, '|'
              ELSE
                  WRITE(10, 11, ERR=101) '        N/D|'
              ENDIF
          ENDIF
              
          WRITE(10, 12, ERR=101)               ! Переход на новую строку
          
          DO J=1, REAL_M, 1
              WRITE(10, 11, ERR=101) '------------'     ! Разделитель между строками
          ENDDO
          WRITE (10, 11, ERR=101) '-------------'       ! Финальный разделитель 
          WRITE(10, 12, ERR=101)
          
          GOTO 109
          
10        FORMAT(E11.4, A, $)                  ! Формат записи значений
11        FORMAT(A, $)                         ! Формат для N/D
12        FORMAT()                             ! Формат endl
   
101       PAUSE 'Write table file error!'
          CLOSE(10)   
          STOP
          
109       CONTINUE 
      END


          
      SUBROUTINE CREATE_TABLE()
          IMPLICIT NONE
          COMMON /INPUT/ X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP
          REAL X_MAX, Y_MAX, X_MIN, Y_MIN, X_STEP, Y_STEP, X, Y
          INTEGER N, M, I, J, REAL_M, IS_INVISIBLE
          
          REAL_M = 1

          ! Установка минимальных значений как начальных
          X = X_MIN    
          Y = Y_MIN
          
          ! Расчет размера матрицы (кол-во шагов)
          N = (X_MAX - X_MIN) / X_STEP + 1
          M = (Y_MAX - Y_MIN) / Y_STEP + 1
          
          ! Последние элементы не точно делятся - добавляем
          IF((X_MIN + (N - 1) * X_STEP).NE.X_MAX) THEN
              N = N + 1 
          ENDIF
          
          IF((Y_MIN + (M - 1) * Y_STEP).NE.Y_MAX) THEN
              M = M + 1
          ENDIF
          
          ! открываем таблицу
          OPEN(10, FILE='table.txt', ERR=100)
                                                         ! Заголовок таблицы
          WRITE(10, 11, ERR=101) '|        X/Y|'         ! Надпись X/Y
          WRITE(10, 10, ERR=101) Y_MIN, '|'              ! Запись минимального Y 
          DO I=1, M-2, 1                                 ! Запись столбца значений Y 
              IF(IS_INVISIBLE(Y, Y + Y_STEP).EQ.1) THEN
                  Y = Y + Y_STEP                         ! Невидимый шаг
                  CYCLE                                  ! Выход из цикла
              ENDIF
              Y = Y + Y_STEP
              IF(ABS(Y).LT.ABS(Y_STEP/2)) THEN
                  Y = 0             
              ENDIF
              WRITE(10, 10, ERR=101) Y, '|'   
              REAL_M = REAL_M + 1                        ! Т.к Y - строки         
          ENDDO
          IF(IS_INVISIBLE(Y, Y_MAX).EQ.0) THEN
              REAL_M = REAL_M + 1
              WRITE(10, 10, ERR=101) Y_MAX, '|'
          ENDIF
          WRITE(10, 12, ERR=101)
          DO J=1, REAL_M, 1
              WRITE(10, 11, ERR=101) '------------'
          ENDDO
          WRITE (10, 11, ERR=101) '-------------'
          WRITE(10, 12, ERR=101)       
          
          CALL WRITE_ROW(X_MIN, M, REAL_M)   ! После создания структуры строк
                                             ! Создаем столбцы
          
          
          !Write from 2 to N-1 row           ! Теперь заполняем строки
          DO I=1, N-2, 1          
              IF(IS_INVISIBLE(X, X + X_STEP).EQ.0) THEN
                  X = X + X_STEP             ! нет невидимого шаг
                  IF(ABS(X).LT.ABS(X_STEP/2)) THEN
                      X = 0                  ! Погрешность
                  ENDIF    
                  CALL WRITE_ROW(X, M, REAL_M) ! вызов заполнения строки x
              ELSE
                  X = X + X_STEP               ! Если есть, переходим на сл строку
              ENDIF
          ENDDO
          
          !Заполнение последней строки
          IF(IS_INVISIBLE(X, X_MAX).EQ.0) THEN
              CALL WRITE_ROW(X_MAX, M, REAL_M) ! Если нет невидимого шага - пишем
          ENDIF
          
          CLOSE(10)
          
          GOTO 109
          
10        FORMAT(E11.4, A, $)  ! Формат для вычислений
11        FORMAT(A, $)         ! Запись строк
12        FORMAT()             ! Переход на новую строку
   
100       PAUSE 'Open table file error!'
          STOP   
   
101       PAUSE 'Write table file error!'
          CLOSE(10)   
          STOP
          
109       CONTINUE   
      END

! Добавить проверку на диапозон суммы [-1, 1]
! ANGLE * π / 180, но у меня ANGLE / 180 * 3.1415926
! ACOS должен возращать так же число из диапозона [-1, 1]