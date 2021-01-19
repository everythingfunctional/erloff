module erloff_message_type_m
    use iso_varying_string, only: varying_string, assignment(=), operator(//)
    use strff, only: to_string

    implicit none
    private
    public :: message_type_t

    type :: message_type_t
        character(len=100) :: description
        logical :: is_fundamental = .false.
    contains
        private
        procedure, public :: to_string => message_type_to_string
        procedure, public :: repr
    end type
contains
    pure function message_type_to_string(self) result(string)
        class(message_type_t), intent(in) :: self
        type(varying_string) :: string

        if (self%is_fundamental) then
            string = ""
        else
            string = trim(self%description) // ": "
        end if
    end function

    pure function repr(self)
        class(message_type_t), intent(in) :: self
        type(varying_string) :: repr

        repr = &
                "message_type_t(description = " // trim(self%description) &
                // ", is_fundamental = " // to_string(self%is_fundamental) // ")"
    end function
end module
