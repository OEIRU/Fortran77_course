program convert_to_binary_vector
    implicit none
    integer :: n, i, io
    real, allocatable :: F(:)

    ! Чтение текстового файла vector.txt
    open(11, file='vector.txt', status='old', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file vector.txt'
        stop
    end if

    ! Чтение размера вектора
    read(11, *, iostat=io) n
    if (io /= 0) then
        print *, 'Error: Failed to read vector size from vector.txt'
        stop
    end if

    allocate(F(n))
    read(11, *, iostat=io) (F(i), i=1, n)
    if (io /= 0) then
        print *, 'Error: Failed to read vector data from vector.txt'
        stop
    end if
    close(11)

    ! Запись вектора в бинарный файл
    open(21, file='vector.bin', status='replace', access='stream', form='unformatted', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file vector.bin for writing'
        stop
    end if

    write(21) n          ! Записываем размер вектора
    write(21) F          ! Записываем данные вектора
    close(21)

    print *, "Vector successfully written to vector.bin"
end program convert_to_binary_vector
