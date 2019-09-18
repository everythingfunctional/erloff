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
        type(Node_t), pointer :: tail => null()
    contains
        private
        procedure, public :: appendMessage
        procedure, public :: appendMessages
        procedure, public :: toString
        final :: destructor
    end type MessageList_t
contains
    subroutine appendMessage(self, message)
        use Message_m, only: Message_t

        class(MessageList_t), intent(inout) :: self
        class(Message_t), pointer, intent(in) :: message

        if (associated(self%head)) then
            allocate(self%tail%next)
            self%tail => self%tail%next
            self%tail%message => message
            nullify(self%tail%next)
        else
            allocate(self%head)
            self%head%message => message
            nullify(self%head%next)
            self%tail => self%head
        end if
    end subroutine appendMessage

    subroutine appendMessages(self, messages)
        class(MessageList_t), intent(inout) :: self
        type(MessageList_t), intent(inout) :: messages

        allocate(self%head)
        self%head => messages%head
        nullify(messages%head)
    end subroutine appendMessages

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = recursiveToString(self%head)
    contains
        recursive function recursiveToString(node) result(string_)
            use iso_varying_string, only: &
                    VARYING_STRING, assignment(=), operator(//)
            use strff, only: NEWLINE

            type(Node_t), pointer :: node
            type(VARYING_STRING) :: string_

            if (associated(node)) then
                string_ = &
                        node%message%toString() &
                        // NEWLINE &
                        // recursiveToString(node%next)
            else
                string_ = ""
            end if
        end function recursiveToString
    end function toString

    subroutine destructor(self)
        type(MessageList_t), intent(inout) :: self

        type(Node_t), pointer :: current

        current => self%head
        do while (associated(current))
            deallocate(current%message)
            current => current%next
        end do
    end subroutine destructor
end module Message_list_m
