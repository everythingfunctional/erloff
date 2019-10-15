module Message_list_m
    use Message_m, only: Message_t

    implicit none
    private

    type, public :: MessageList_t
        private
        class(Message_t), allocatable :: message
    contains
        private
        procedure, public :: appendMessage
        procedure, public :: toString
    end type MessageList_t
contains
    subroutine appendMessage(self, message)
        use Message_m, only: Message_t

        class(MessageList_t), intent(inout) :: self
        class(Message_t), intent(in) :: message

        allocate(self%message, source = message)
    end subroutine appendMessage

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING) :: string

        if (allocated(self%message)) then
            string = self%message%toString()
        else
            string = ""
        end if
    end function toString
end module Message_list_m
