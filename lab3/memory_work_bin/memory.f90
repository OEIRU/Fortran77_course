module matrix_multiply
    implicit none
    integer, parameter :: max_size = 1000000
    integer :: n, i1, i2
    real :: A(5, max_size)  ! Хранение диагоналей в двумерном массиве
    real :: F(max_size), result(max_size)
    common /razm/ n, i1, i2

contains

subroutine read_matrix(filename)
    character(len=*), intent(in) :: filename
    integer :: io, i
    
    open(10, file=filename, access='direct', recl=4, iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file ', filename
        stop
    end if

    read(10, rec=1, iostat=io) n
    read(10, rec=2, iostat=io) i1
    read(10, rec=3, iostat=io) i2
    if (io /= 0) then
        print *, 'Error: Failed to read dimensions'
        stop
    end if
    
    ! Чтение всех диагоналей
    do i = 1, n
        read(10, rec=3+i, iostat=io) A(1, i)  ! Главная диагональ
    end do
    do i = 1, n-1
        read(10, rec=3+n+i, iostat=io) A(2, i)  ! Первая верхняя
    end do
    do i = 1, n-i2
        read(10, rec=3+n+(n-1)+i, iostat=io) A(3, i)  ! Вторая верхняя
    end do
    do i = 1, n-1
        read(10, rec=3+n+(n-1)+(n-i2)+i, iostat=io) A(4, i)  ! Первая нижняя
    end do
    do i = 1, n-i1
        read(10, rec=3+n+(n-1)+(n-i2)+(n-1)+i, iostat=io) A(5, i)  ! Вторая нижняя
    end do

    if (io /= 0) then
        print *, 'Error: Failed to read matrix data'
        stop
    end if
    close(10)

    ! Отладочный вывод
    print *, "A after reading:"
    do i = 1, 5
        if (i == 1) then
            print *, A(i, 1:n)
        else if (i == 2 .or. i == 4) then
            print *, A(i, 1:n-1)
        else if (i == 3) then
            print *, A(i, 1:n-i2)
        else if (i == 5) then
            print *, A(i, 1:n-i1)
        end if
    end do
end subroutine read_matrix

subroutine read_vector(filename)
    character(len=*), intent(in) :: filename
    integer :: io, i
    
    open(11, file=filename, access='direct', recl=4, iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file ', filename
        stop
    end if

    read(11, rec=1, iostat=io) n
    if (io /= 0) then
        print *, 'Error: Failed to read vector size'
        stop
    end if

    do i = 1, n
        read(11, rec=1+i, iostat=io) F(i)
    end do
    if (io /= 0) then
        print *, 'Error: Failed to read vector data'
        stop
    end if
    close(11)

    print *, "F before multiplication:", F(1:n)
end subroutine read_vector

subroutine multiply_matrix_vector()
    integer :: i, j
    
    do i = 1, n
        result(i) = A(1, i) * F(i)
        
        if (i > 1) result(i) = result(i) + A(4, i-1) * F(i-1)  ! Первая нижняя диагональ
        if (i > 2) result(i) = result(i) + A(5, i-2) * F(i-2)  ! Вторая нижняя диагональ
        
        if (i < n) result(i) = result(i) + A(2, i) * F(i+1)  ! Первая верхняя диагональ
        if (i <= n-i2) result(i) = result(i) + A(3, i) * F(i+i2)  ! Вторая верхняя диагональ

        
        print *, "Result after ", i, "th iteration: ", result(i)
    end do

    F(1:n) = result(1:n)
end subroutine multiply_matrix_vector

subroutine write_vector(filename)
    character(len=*), intent(in) :: filename
    integer :: io
    
    open(12, file=filename, status='replace', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot write to file ', filename
        stop
    end if
    
    write(12, *) F(1:n)
    close(12)
end subroutine write_vector

end module matrix_multiply

program main
    use matrix_multiply
    
    call read_matrix('matrix.bin')
    call read_vector('vector.bin')
    call multiply_matrix_vector()
    call write_vector('result.txt')
    
    print *, 'Multiplication completed successfully.'
    print *, 'Result vector: ', F(1:n)
end program main