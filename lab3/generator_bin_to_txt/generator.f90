program bin_to_txt_converter
    implicit none
    integer :: n, i1, i2, io, i
    real :: value

    ! Чтение и декодирование matrix.bin
    open(10, file='matrix.bin', access='direct', recl=KIND(value), iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file matrix.bin'
        stop
    end if

    open(11, file='matrix_decoded.txt', status='replace', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot create file matrix_decoded.txt'
        stop
    end if

    read(10, rec=1, iostat=io) n
    read(10, rec=2, iostat=io) i1
    read(10, rec=3, iostat=io) i2
    if (io /= 0) then
        print *, 'Error reading matrix dimensions from matrix.bin'
        stop
    end if

    write(11, *) n, i1, i2
    
    do i = 1, n
        read(10, rec=3+i, iostat=io) value
        if (io /= 0) exit
        write(11, *) value
    end do
    do i = 1, n-1
        read(10, rec=3+n+i, iostat=io) value
        if (io /= 0) exit
        write(11, *) value
    end do
    do i = 1, n-i2
        read(10, rec=3+n+(n-1)+i, iostat=io) value
        if (io /= 0) exit
        write(11, *) value
    end do
    do i = 1, n-1
        read(10, rec=3+n+(n-1)+(n-i2)+i, iostat=io) value
        if (io /= 0) exit
        write(11, *) value
    end do
    do i = 1, n-i1
        read(10, rec=3+n+(n-1)+(n-i2)+(n-1)+i, iostat=io) value
        if (io /= 0) exit
        write(11, *) value
    end do

    close(10)
    close(11)

    ! Чтение и декодирование vector.bin
    open(20, file='vector.bin', access='direct', recl=KIND(value), iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot open file vector.bin'
        stop
    end if

    open(21, file='vector_decoded.txt', status='replace', iostat=io)
    if (io /= 0) then
        print *, 'Error: Cannot create file vector_decoded.txt'
        stop
    end if

    read(20, rec=1, iostat=io) n
    if (io /= 0) then
        print *, 'Error reading vector size from vector.bin'
        stop
    end if

    write(21, *) n

    do i = 1, n
        read(20, rec=1+i, iostat=io) value
        if (io /= 0) exit
        write(21, *) value
    end do

    if (io /= 0) then
        print *, 'Error reading vector data from vector.bin'
    end if

    close(20)
    close(21)

    print *, 'Decoding completed successfully.'
end program bin_to_txt_converter