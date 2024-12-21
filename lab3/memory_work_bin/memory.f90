module matrix_multiply_binary
    implicit none
    integer, parameter :: max_size = 1000000
    integer :: n, i1, i2
    real :: A(5, max_size)  ! Хранение диагоналей в двумерном массиве
    real :: F(max_size), result(max_size)
    common /razm/ n, i1, i2

contains

subroutine read_matrix_binary(filename)
    character(len=*), intent(in) :: filename
    integer :: io, i
    
    open(10, file=filename, status='old', access='stream', form='unformatted', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file ', filename
        stop
    end if

    read(10) n, i1, i2
    read(10) A
    close(10)

    ! Отладочный вывод
    print *, "A after reading from binary:"
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
end subroutine read_matrix_binary

subroutine read_vector_binary(filename)
    character(len=*), intent(in) :: filename
    integer :: io, i
    
    open(11, file=filename, status='old', access='stream', form='unformatted', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file ', filename
        stop
    end if

    read(11) n
    read(11) F
    close(11)

    print *, "F before multiplication from binary:", F(1:n)
end subroutine read_vector_binary


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

program main_binary
    use matrix_multiply_binary
    
    call read_matrix_binary('matrix.bin')
    call read_vector_binary('vector.bin')
    call multiply_matrix_vector()
    call write_vector('result.bin')
    
    print *, 'Multiplication completed successfully.'
    print *, 'Result vector: ', F(1:n)
end program main_binary