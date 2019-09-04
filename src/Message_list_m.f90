module Message_list_m
    use Message_m, only: Message_t

    implicit none
    private

    type :: MessageItem_t
        class(Message_t), allocatable :: message
    contains
        procedure :: toString => itemToString
    end type MessageItem_t

    type, public :: MessageList_t
        private
        type(MessageItem_t), allocatable :: messages(:)
    contains
        private
        procedure, public :: appendMessage
        procedure, public :: appendMessages
        procedure, public :: toString
    end type MessageList_t

contains
    pure function appendMessage(self, message) result(new_list)
        use Message_m, only: Message_t

        class(MessageList_t), intent(in) :: self
        class(Message_t), intent(in) :: message
        type(MessageList_t) :: new_list

        integer :: num_old_messages
        integer :: num_messages

        if (allocated(self%messages)) then
            num_old_messages = size(self%messages)
            num_messages = num_old_messages + 1
            allocate(new_list%messages(num_messages))
            new_list%messages(1:num_old_messages) = self%messages(:)
            allocate(new_list%messages(num_messages)%message, source = message)
        else
            allocate(new_list%messages(1))
            allocate(new_list%messages(1)%message, source = message)
        end if
    end function appendMessage

    pure function appendMessages(self, messages) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(MessageList_t), intent(in) :: messages
        type(MessageList_t) :: new_list

        associate(a => self); end associate

        new_list = messages
    end function appendMessages

    pure function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)
        use strff, only: join, NEWLINE

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING) :: string

        if (allocated(self%messages)) then
            string = join(self%messages%toString(), NEWLINE)
        else
            string = ""
        end if
    end function toString

    elemental function itemToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(MessageItem_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%message%toString()
    end function itemToString
end module Message_list_m
