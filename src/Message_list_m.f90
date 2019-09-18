module Message_list_m
    use Message_m, only: Message_t

    implicit none
    private

    type :: Node_t
        class(Message_t), pointer :: message
        type(Node_t), pointer :: next
    end type Node_t

    type, public :: MessageList_t
        private
        type(Node_t), pointer :: head => null()
    contains
        private
        procedure, public :: appendMessage
        procedure, public :: toString
    end type MessageList_t
contains
    subroutine appendMessage(self, message)
        use Message_m, only: Message_t

        class(MessageList_t), intent(inout) :: self
        class(Message_t), pointer, intent(in) :: message

        allocate(self%head)
        self%head%message => message
        nullify(self%head%next)
    end subroutine appendMessage

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING) :: string

        if (associated(self%head)) then
            string = self%head%message%toString()
        else
            string = ""
        end if
    end function toString
end module Message_list_m
