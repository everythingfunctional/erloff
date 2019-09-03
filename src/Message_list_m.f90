module Message_list_m
    use Message_m, only: Message_t

    implicit none
    private

    type :: MessageItem_t
        class(Message_t), allocatable :: message
    end type MessageItem_t

    type, public :: MessageList_t
        private
        type(MessageItem_t), allocatable :: messages(:)
    contains
        private
        procedure, public :: appendMessage
        procedure, public :: toString
    end type MessageList_t

contains
    pure function appendMessage(self, message) result(new_list)
        use Message_m, only: Message_t

        class(MessageList_t), intent(in) :: self
        class(Message_t), intent(in) :: message
        type(MessageList_t) :: new_list

        associate(a => self); end associate

        allocate(new_list%messages(1))
        allocate(new_list%messages(1)%message, source = message)
    end function appendMessage

    pure function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING) :: string

        if (allocated(self%messages)) then
            string = self%messages(1)%message%toString()
        else
            string = ""
        end if
    end function toString
end module Message_list_m
