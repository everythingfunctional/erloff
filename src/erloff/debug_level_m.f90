module erloff_debug_level_m
    use iso_varying_string, only: varying_string
    use strff, only: to_string

    implicit none
    private
    public :: debug_level_t, GENERAL, MEDIUM, DETAILED, NITTY_GRITTY

    type :: debug_level_t
        private
        integer :: level
    contains
        private
        procedure, public :: to_string => debug_level_to_string
    end type

    type(debug_level_t), parameter :: GENERAL = debug_level_t(1)
    type(debug_level_t), parameter :: MEDIUM = debug_level_t(2)
    type(debug_level_t), parameter :: DETAILED = debug_level_t(3)
    type(debug_level_t), parameter :: NITTY_GRITTY = debug_level_t(4)
contains
    pure function debug_level_to_string(self) result(string)
        class(debug_level_t), intent(in) :: self
        type(varying_string) :: string

        string = to_string(self%level)
    end function
end module
