!gfortran practica1.f90 -o practica1; ./practica1
!cd "C:\Cursos\Fortran\Lab LFP\practica1"


!C:\Cursos\Fortran\Lab_LFP\practica1\inventario.inv

!gfortran -ffree-line-length-512  practica1.f90 -o practica1; ./practica1   


module varios
    
    implicit none

    !declaracion de variable global de array
    integer  :: contador = 0
    integer  :: contador2 = 0

    !////////////////////////////////////////////////////////////////
    !CLASES
    !////////////////////////////////////////////////////////////////
    
    Type :: producto
        character(len = 100) :: nombre, ubicacion
        integer :: cantidad
        real :: precio_u
        
    end Type producto

     type(producto), allocatable :: lista(:)

    
    !////////////////////////////////////////////////////////////////
    !////////////////////////////////////////////////////////////////


    contains
   
   !creando la clase falsa

   subroutine cproducto(st,nombre,cantidad,precio_u,ubicacion)
        Type(producto), intent(out) :: st
        character(len = *) :: nombre
        character(len = *) :: ubicacion
        integer :: cantidad,i
        real :: precio_u
        

        !esto es como un constructor, con su this y todo
        st%nombre = nombre
        st%ubicacion = ubicacion
        st%cantidad = cantidad
        st%precio_u = precio_u

    end subroutine cproducto




    subroutine menu(opcion)

        INTEGER, intent(inout) :: opcion
        INTEGER ::  ios_1
        

        call system ("cls")
        write(*, '(A)') char(27)//'[1;31m'//" OPCION INCORRECTA, INTENTE DE NUEVO "//char(27)//'[0m'
        print*, '--------------------------------------------------------'
        print*, 'Practica 1 - Lenguajes formales y de programacion '
        print*, '--------------------------------------------------------'
        print*, '#Sistema de Inventario'
        print*,''
        print*, '1. Cargar inventario inicial'
        print*, '2. Cargar instrucciones de movimientos'
        print*, '3. Crear informe de inventario actual'
        print*, '4. Salir'
        print*,''
        print*,''
        print*,''
        
        write(*, '(A)', advance='no') 'Ingrese una opcion >>>>>> ' 
        read(*,*, iostat = ios_1) opcion

   

    end subroutine menu



subroutine menu2(opcion)

        INTEGER, intent(inout) :: opcion
        INTEGER ::  ios_1
        

        call system ("cls")
        print*, '--------------------------------------------------------'
        print*, 'Practica 1 - Lenguajes formales y de programacion '
        print*, '--------------------------------------------------------'
        print*, '#Sistema de Inventario'
        print*,''
        print*, '1. Cargar inventario inicial'
        print*, '2. Cargar instrucciones de movimientos'
        print*, '3. Crear informe de inventario actual'
        print*, '4. Salir'
        print*,''
        print*,''
        print*,''
        
        write(*, '(A)', advance='no') 'Ingrese una opcion >>>>>> ' 
        read(*,*, iostat = ios_1) opcion

   

    end subroutine menu2






     !se crea la lectura del archivo 
    subroutine inv(nombre)
        character(*), intent(inout) :: nombre
        character(len=500) :: comando, detalles
        integer :: ios,ios_inv,start,endpos
        character(len=500) :: linea

        !declaracion de variables para la 2da separacion
        integer :: i,start_2,endpos_2
        character (len=100) :: field(4), temp_line

        character (len=50) :: temp_nombre, temp_ubicacion, temp_contador
        integer :: temp_cantidad
        real :: temp_precio_u

        !declaracion de nombre de objeto de la clase
        
        logical :: termino = .false.


        !contador para el tamaño del array que guarda cada objeto
        

        

        allocate(lista(100))


        open(unit=1, file=nombre, status='old', action='read', iostat=ios)


        if (ios /= 0) then
        write(*,*) "Error al abrir el archivo"
        stop
    else
        
        do while (termino .neqv. .true.)
            contador = contador + 1
            !para leer mas de una linea se usa el "(A)"  
            read(1, "(A)" ,iostat=ios) linea
            if (ios /= 0) then
            print*,"finalizo el programa o hay algun error"
               termino = .true.
            
            else 
                start = 1
                endpos = scan(linea(start:), ' ')
                if (endpos == 0) then
                    comando = trim(linea(start:))
                    detalles = ''
                    
                else
                    comando = trim(linea(start:start+endpos-2))
                    detalles = trim(linea(start+endpos:))
                end if
                write(*,*) " "
                write(*,*) "contenido:"
                print*,"comando: ", trim(comando)
                print*, "datos: ", trim(detalles)

            end if
  

            !separar los datos
           temp_line = detalles
           start_2 = 1

           do i=1,4
                endpos_2 = index(temp_line(start_2:), ';')
                if (endpos_2 == 0 .and. i==4) then
                    field(i) = trim(temp_line(start_2:))
                else
                    field(i) = trim(temp_line(start_2:start_2+endpos_2-2))
                    start_2 = start_2 + endpos_2
                end if

           end do


           if (termino .neqv. .true.) then
            !aqui existe el array con los datos separados pero todos son characters
            !ahora se deben convertir a su tipo de dato correspondiente
            temp_nombre = field(1)
            read(field(2),'(I15)') temp_cantidad

            read(field(3),*) temp_precio_u
            temp_ubicacion = field(4)
            
            
            

            !pasar a texto el contador para ponerlo al nombre
            
            lista(contador)%nombre = temp_nombre
            lista(contador)%cantidad = temp_cantidad
            lista(contador)%precio_u = temp_precio_u
            lista(contador)%ubicacion = temp_ubicacion

            write(temp_contador, '(I0)') contador

            print*, "se agrego a la lista en posicion: ", trim(temp_contador), " el producto: ", lista(contador)%nombre

            else
            
            end if
            
        end do

    end if
    
    end subroutine inv






    subroutine inv2(nombre2)
        character(*), intent(inout) :: nombre2
        character(len=500) :: comando, detalles
        integer :: ios3,ios_inv,start,endpos
        character(len=500) :: linea

        !declaracion de variables para la 2da separacion
        integer :: i,start_2,endpos_2
        character (len=100) :: field(3), temp_line

        character (len=50) :: temp_nombre, temp_ubicacion, temp_contador
        integer :: temp_cantidad
        real :: temp_precio_u

        !declaracion de nombre de objeto de la clase
        
        logical :: termino = .false.


        !contador para el tamaño del array que guarda cada objeto
        

        




        open(unit=3, file=nombre2, status='old', action='read', iostat=ios3)


        if (ios3 /= 0) then
        write(*,*) "Error al abrir el archivo"
        stop
    else
        
        do while (termino .neqv. .true.)
            contador2 = contador2 + 1
            !para leer mas de una linea se usa el "(A)"  
            read(3, "(A)" ,iostat=ios3) linea

            if (ios3 /= 0) then
                print*,"finalizo el programa o hay algun error"
                termino = .true.
            
            else 
                start = 1
                endpos = scan(linea(start:), ' ')
                if (endpos == 0) then
                    comando = trim(linea(start:))
                    detalles = ''
                    
                else
                    comando = trim(linea(start:start+endpos-2))
                    detalles = trim(linea(start+endpos:))
                end if
                write(*,*) " "
                write(*,*) " "
                write(*,*) " "
                write(*,*) "contenido:"
                print*,"comando: ", trim(comando)
                print*, "datos: ", trim(detalles)
                print*, ""
            end if
  
            
            !separar los datos
           temp_line = detalles
           start_2 = 1

           do i=1,3
                endpos_2 = index(temp_line(start_2:), ';')
                if (endpos_2 == 0 .and. i==3) then
                    field(i) = trim(temp_line(start_2:))
                else
                    field(i) = trim(temp_line(start_2:start_2+endpos_2-2))
                    start_2 = start_2 + endpos_2
                end if

           end do


           if (termino .neqv. .true.) then
            !aqui existe el array con los datos separados pero todos son characters
            !ahora se deben convertir a su tipo de dato correspondiente
            print*, "comando ", trim(comando)
            print*, "nombre ", trim(field(1))
            print*, "cantidad ", trim(field(2))
            print*, "ubicacion ", trim(field(3))

        temp_nombre = field(1)
        read(field(2),'(I5)') temp_cantidad
        temp_ubicacion = field(3)


        if (trim(comando) == "agregar_stock") then
            !buscar el producto en la lista
            do i = 1, contador-1
                if (lista(i)%nombre == temp_nombre .and. lista(i)%ubicacion == temp_ubicacion) then
                    !sumar la cantidad
                    lista(i)%cantidad = lista(i)%cantidad + temp_cantidad
                    print*, "se agrego ", temp_cantidad, " de ", trim(temp_nombre), " en ", trim(temp_ubicacion)
                    print*, ""
                end if
            end do

        else if (trim(comando) == "eliminar_equipo") then
            !buscar el producto en la lista
            do i = 1, contador-1
                if (lista(i)%nombre == temp_nombre .and. lista(i)%ubicacion == temp_ubicacion .and. lista(i)%cantidad >= temp_cantidad) then
                    !restar la cantidad
                    lista(i)%cantidad = lista(i)%cantidad - temp_cantidad
                    print*, "se elimino ", temp_cantidad, " de ", trim(temp_nombre), " en ", trim(temp_ubicacion)
                end if
            end do 

        else 
        print*, "comando no valido"      
        end if

            

            else
            !no hacer nada
            end if

        end do

    end if
    
    end subroutine inv2

!recorrer la lista en nombre y ubicación
!




    


end module varios







program practica1
    use varios

    implicit none
    CHARACTER(500) :: nombre, nombre2
    INTEGER :: ios, ios_1,i
    INTEGER :: opcion = 5
    character(500) :: informe
    

    
        call system ("cls")
    
        print*, '--------------------------------------------------------'
        print*, 'Practica 1 - Lenguajes formales y de programacion'
        print*, '--------------------------------------------------------'
        print*, '#Sistema de Inventario'
        print*,''
        print*, '1. Cargar inventario inicial'
        print*, '2. Cargar instrucciones de movimientos'
        print*, '3. Crear informe de inventario actual'
        print*, '4. Salir'
        print*,''
        print*,''
        print*,''
        
        write(*, '(A)', advance='no') 'Ingrese una opcion >>>>>> ' 
        
        read(*,*, iostat = ios_1) opcion 

    do 
        if(opcion /=1 .and. opcion /=2 .and. opcion /=3 .and. opcion /=4 .and. opcion /=0) then

            call menu(opcion)

            else if ( opcion == 0 ) then

                call menu2(opcion)

                else 
            

                select case(opcion)
                    case(1)
                    if (contador /= 0) then
                        call system ("cls")
                        print*, 'Ya se ha cargado un inventario inicial'

                    else
                        call system ("cls")
                        print*, '--------------------------------------------------------'
                        write(*, '(A)') char(27)//'[1m'//"                 CARGAR INVENTARIO INICIAL "//char(27)//'[0m'
                        print*, '--------------------------------------------------------'
                        print*,''
                        print*,''
                        print*,''
                        write(*, '(A)', advance='no') 'Ingrese la ruta del inventario: >>>>>> ' 
                        read(*,*, iostat = ios_1) nombre

                        
                        call inv(nombre)
                    
                    print*,("hola")

                    do i = 1, contador-1

                        print *, 'Producto:', i
                        print *, 'Nombre:', trim(lista(i)%nombre)
                        print *, 'cantidad:', lista(i)%cantidad
                        print *, 'precisso:', lista(i)%precio_u
                        print *, 'ubicacion:', lista(i)%ubicacion

                        

                    end do

                    end if

                        

                    
                    case(2)
                        call system ("cls")
                        print*, '--------------------------------------------------------'
                        write(*, '(A)') char(27)//'[1m'//"          CARGAR INSTRUCCIONES DE MOVIMIENTO "//char(27)//'[0m'
                        print*, '--------------------------------------------------------'
                        if (contador == 0) then
                            print*, 'No se ha cargado ningun inventario inicial'
                        else
                            print*,''
                            print*,''
                            print*,''
                            write(*, '(A)', advance='no') 'Ingrese la ruta del inventario: >>>>>> ' 
                            read(*,*, iostat = ios_1) nombre2

                            call inv2(nombre2)
                        end if


                    case(3)
                        call system ("cls")
                        print*, '--------------------------------------------------------'
                        write(*, '(A)') char(27)//'[1m'//"          CREAR INFORME DE INVENTARIO FINAL "//char(27)//'[0m'
                        print*, '--------------------------------------------------------'

                         informe = "C:\Cursos\Fortran\Lab_LFP\practica1\informe.txt"
                        open(unit=2, file=informe, status='replace', action='write', iostat=ios)
                        
                        
                          write(2,'(A)') "No.         Producto      Cantidad           P. unit             Total           Ubicacion"
                           write(2,'(A)') "---------------------------------------------------------------------------------"
                         do i = 1, contador-1

                            write(2, '(I2,A10,A13, I5,A13)',ADVANCE="NO") i,'', lista(i)%nombre, lista(i)%cantidad,''
                            write(2, '(F9.2,A10,F9.2, A10)',ADVANCE="NO") lista(i)%precio_u,'',lista(i)%precio_u*lista(i)%cantidad 
                            write(2, '(A10,A10)')'', lista(i)%ubicacion

                        end do
                        
                        
                        
                        
                    case(4)
                        call system ("cls")
                        write(*, '(A)') char(27)//'[1m'//"adios... "//char(27)//'[0m'
                        stop
                    case default
                        print*, 'Opcion incorrecta'
                
                end select

                write(*, '(A)') char(27)//'[1m'//"Operacion terminada... Volviendo al menu principal "//char(27)//'[0m'
                opcion = 0
                call sleep(10)


            end if
        

    

    end do



    





    

    
end program practica1





