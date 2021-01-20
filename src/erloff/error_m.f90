module erloff_error_m
    use erloff_message_m, only: message_t
    use erloff_message_type_m, only: message_type_t

    implicit none
    private
    public :: error_t, ERROR, ERROR_TYPE_STRING

    type, abstract, extends(message_t) :: error_t
    end type

    character(len=*), parameter :: ERROR_TYPE_STRING = "error_t"
    type(message_type_t), parameter :: ERROR = message_type_t( &
            ERROR_TYPE_STRING, .true.)
end module
