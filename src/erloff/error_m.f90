module erloff_error_m
    use erloff_message_m, only: message_t, default_is_type
    use erloff_message_type_m, only: message_type_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use iso_varying_string, only: varying_string, var_str

    implicit none
    private
    public :: error_t, error_is_type, ERROR

    type, abstract, extends(message_t) :: error_t
    contains
        private
        procedure, public :: is_type => error_is_type
        procedure(prepend_names_i), public, deferred :: with_names_prepended_
        procedure(add_content_i), public, deferred :: with_content_appended_s_
        procedure, public :: with_content_appended_c_
        generic, public :: with_content_appended_ => &
                with_content_appended_c_, with_content_appended_s_
        procedure(add_content_i), public, deferred :: with_content_prepended_s_
        procedure, public :: with_content_prepended_c_
        generic, public :: with_content_prepended_ => &
                with_content_prepended_c_, with_content_prepended_s_
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

        function add_content_i(self, string) result(new_error)
            import :: error_t, varying_string

            implicit none

            class(error_t), intent(in) :: self
            type(varying_string), intent(in) :: string
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

    function with_content_appended_c_(self, string) result(new_error)
        class(error_t), intent(in) :: self
        character(len=*), intent(in) :: string
        class(error_t), allocatable :: new_error

        new_error = self%with_content_appended_(var_str(string))
    end function

    function with_content_prepended_c_(self, string) result(new_error)
        class(error_t), intent(in) :: self
        character(len=*), intent(in) :: string
        class(error_t), allocatable :: new_error

        new_error = self%with_content_prepended_(var_str(string))
    end function
end module
