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
        procedure, public :: appendMessages
        procedure, public :: toString
    end type MessageList_t
contains
    subroutine appendMessage(self, message)
        use Message_m, only: Message_t

        class(MessageList_t), intent(inout) :: self
        class(Message_t), intent(in) :: message

        type(MessageItem_t) :: old_messages(size(self%messages))

        if (allocated(self%messages)) then
            old_messages = self%messages
            deallocate(self%messages)
            allocate(self%messages(size(old_messages) + 1))
            self%messages(1:size(old_messages)) = old_messages
            allocate(self%messages(size(self%messages))%message, source = message)
        else
            allocate(self%messages(1))
            allocate(self%messages(1)%message, source = message)
        end if
    end subroutine appendMessage

    subroutine appendMessages(self, messages, module_, procedure_)
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        class(MessageList_t), intent(inout) :: self
        type(MessageList_t), intent(in) :: messages
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_

        integer :: i
        type(MessageItem_t) :: old_messages(size(self%messages))
        integer :: num_new_messages
        integer :: num_old_messages
        integer :: total_num_messages

        if (allocated(messages%messages)) then
            if (allocated(self%messages)) then
                num_old_messages = size(self%messages)
                num_new_messages = size(messages%messages)
                total_num_messages = num_old_messages + num_new_messages
                old_messages = self%messages
                deallocate(self%messages)
                allocate(self%messages(total_num_messages))
                self%messages(1:num_old_messages) = old_messages
                self%messages(num_old_messages+1:) = messages%messages
                do i = num_old_messages + 1, total_num_messages
                    call self%messages(i)%message%prependNames(module_, procedure_)
                end do
            else
                allocate(self%messages, source = messages%messages)
                do i = 1, size(self%messages)
                    call self%messages(i)%message%prependNames(module_, procedure_)
                end do
            end if
        end if
    end subroutine appendMessages

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)
        use strff, only: join, NEWLINE

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING) :: string

        integer :: i
        type(VARYING_STRING) :: strings(size(self%messages))

        if (allocated(self%messages)) then
            do i = 1, size(self%messages)
                strings(i) = self%messages(i)%message%toString()
            end do
            string = join(strings, NEWLINE)
        else
            string = ""
        end if
    end function toString
end module Message_list_m
