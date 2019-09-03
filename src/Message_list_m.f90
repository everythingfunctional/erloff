module Message_list_m
    implicit none
    private

    type, public :: MessageList_t
    contains
        private
        procedure, public :: toString
    end type MessageList_t

contains
    pure function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING) :: string

        associate(a => self); end associate

        string = ""
    end function toString
end module Message_list_m
