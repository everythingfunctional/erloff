module erloff_error_m
    use erloff_message_m, only: message_t, default_is_type
    use erloff_message_type_m, only: message_type_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t

    implicit none
    private
    public :: error_t, error_is_type, ERROR

    type, abstract, extends(message_t) :: error_t
    contains
        private
        procedure, public :: is_type => error_is_type
        procedure(prepend_names_i), public, deferred :: with_names_prepended_
    end type

    abstract interface
        function prepend_names_i(self, module_, procedure_) result(new_error)
            import :: error_t, module_t, procedure_t

            implicit none

            class(error_t), intent(in) :: self
            type(module_t), intent(in) :: module_
            type(procedure_t), intent(in) :: procedure_
            class(error_t), allocatable :: new_error
        end function
    end interface

    character(len=*), parameter :: ERROR_TYPE_STRING = "error_t"
    type(message_type_t), parameter :: ERROR = message_type_t( &
            ERROR_TYPE_STRING, .true.)
contains
    pure function error_is_type(self, type_tag) result(is_type)
        class(error_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        logical :: is_type

        if (trim(type_tag%description) == ERROR_TYPE_STRING) then
            is_type = .true.
        else
            is_type = default_is_type(self, type_tag)
        end if
    end function
end module
