program converter
    real :: x
    integer :: k, upper_offset, lower_offset, i, io, element_count

    ! Открываем файл matrix.txt
    open(10,FILE='matrix.txt',STATUS='old',iostat=io)
    if (io /= 0) then
        print *, 'Error opening matrix.txt'
        stop
    endif
    read(10,*,iostat=io) k, upper_offset, lower_offset
    if (io /= 0) then
        print *, 'Error reading matrix dimensions'
        stop
    endif

    print *, 'Conversion...'

    ! Перевод матрицы в двоичный вид
    open(20,FILE='matrix.bin',ACCESS='direct',RECL=KIND(x),iostat=io)
    if (io /= 0) then
        print *, 'Error opening matrix.bin'
        stop
    endif
    write(20,rec=1) k
    write(20,rec=2) upper_offset
    write(20,rec=3) lower_offset

    element_count = 0
    do
        read(10,*,iostat=io) x
        if (io /= 0) exit
        element_count = element_count + 1
        if (element_count > k*5) then  ! Предполагаем, что у нас 5 диагоналей
            print *, 'Warning: More matrix elements than expected!'
            exit
        endif
        write(20,rec=3+element_count) x
    end do

    close(10)
    close(20)

    ! Чтение и запись вектора
    open(11,FILE='vector.txt',STATUS='old',iostat=io)
    if (io /= 0) then
        print *, 'Error opening vector.txt'
        stop
    endif
    read(11,*,iostat=io) k
    if (io /= 0) then
        print *, 'Error reading vector size'
        stop
    endif

    open(21,FILE='vector.bin',ACCESS='direct',RECL=KIND(x),iostat=io)
    if (io /= 0) then
        print *, 'Error opening vector.bin'
        stop
    endif
    write(21,rec=1) k

    element_count = 0
    do
        read(11,*,iostat=io) x
        if (io /= 0) exit
        element_count = element_count + 1
        if (element_count > k) then  ! Проверка на количество элементов вектора
            print *, 'Warning: More vector elements than expected!'
            exit
        endif
        write(21,rec=1+element_count) x
    end do

    close(11)
    close(21)

    print *, 'Conversion complete!'
end program converter