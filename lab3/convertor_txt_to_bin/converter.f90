program convert_to_binary
    implicit none
    integer :: n, i1, i2, i, io
    real, allocatable :: A(:,:), F(:)
    
    ! Конвертация matrix.txt -> matrix.bin
    open(10, file='matrix.txt', status='old', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file matrix.txt'
        stop
    end if
    
    read(10, *, iostat=io) n, i1, i2
    if (io /= 0) then
        print *, 'Error: Failed to read dimensions from matrix.txt'
        stop
    end if
    
    allocate(A(5, n))
    A = 0.0
    
    read(10, *, iostat=io) (A(1, i), i=1, n)    ! Главная диагональ
    read(10, *, iostat=io) (A(2, i), i=1, n-1)  ! Первая верхняя
    read(10, *, iostat=io) (A(3, i), i=1, n-i2) ! Вторая верхняя
    read(10, *, iostat=io) (A(4, i), i=1, n-1)  ! Первая нижняя
    read(10, *, iostat=io) (A(5, i), i=1, n-i1) ! Вторая нижняя
    
    close(10)

    open(20, file='matrix.bin', status='replace', access='stream', form='unformatted', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file matrix.bin for writing'
        stop
    end if
    
    write(20) n, i1, i2
    write(20) A
    close(20)

    ! Конвертация vector.txt -> vector.bin
    open(11, file='vector.txt', status='old', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file vector.txt'
        stop
    end if
    
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

    open(21, file='vector.bin', status='replace', access='stream', form='unformatted', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file vector.bin for writing'
        stop
    end if
    
    write(21) n
    write(21) F
    close(21)
    
    deallocate(A, F)
    print *, 'Conversion completed successfully!'
end program convert_to_binary
