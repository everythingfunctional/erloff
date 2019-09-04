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
        procedure :: prependNamesCC
        generic, public :: prependNames => prependNamesCC
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

        integer :: num_new
        integer :: num_old
        integer :: total_num

        if (allocated(self%messages)) then
            if (allocated(messages%messages)) then
                num_old = size(self%messages)
                num_new = size(messages%messages)
                total_num = num_old + num_new
                allocate(new_list%messages(total_num))
                new_list%messages(1:num_old) = self%messages(:)
                new_list%messages(num_old+1:total_num) = messages%messages(:)
            else
                new_list = self
            end if
        else
            new_list = messages
        end if
    end function appendMessages

    pure function prependNamesCC( &
            self, module_name, procedure_name) result(messages)
        class(MessageList_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(MessageList_t) :: messages

        integer :: i
        integer :: num_messages

        num_messages = size(messages%messages)
        allocate(messages%messages(num_messages))
        do i = 1, num_messages
            allocate(messages%messages(i)%message, source = &
                    self%messages(i)%message%prependNames( &
                            module_name, procedure_name))
        end do
    end function prependNamesCC

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
