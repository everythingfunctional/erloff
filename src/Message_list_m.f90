module Message_list_m
    use Message_m, only: Message_t

    implicit none
    private

    type :: MessageItem_t
        class(Message_t), allocatable :: message
    end type MessageItem_t

    type, public :: MessageList_t
        private
        integer :: length = 0
        type(MessageItem_t), allocatable :: messages(:)
    contains
        private
        procedure, public :: appendMessage
        procedure, public :: appendMessages
        procedure :: ofType
        generic, public :: operator(.ofType.) => ofType
        procedure :: ofTypes
        generic, public :: operator(.ofTypes.) => ofTypes
        procedure :: originatingFromModule
        procedure :: originatingFromModules
        procedure :: originatingFromProcedure
        procedure :: originatingFromProcedures
        generic, public :: operator(.originatingFrom.) => &
                originatingFromModule, &
                originatingFromModules, &
                originatingFromProcedure, &
                originatingFromProcedures
        procedure, public :: toString
    end type MessageList_t

    interface size
        module procedure messageListSize
    end interface size

    public :: size
contains
    subroutine appendMessage(self, message)
        use Message_m, only: Message_t

        class(MessageList_t), intent(inout) :: self
        class(Message_t), intent(in) :: message

        type(MessageItem_t) :: old_messages(self%length)

        if (self%length == 0) then
            allocate(self%messages(1))
            allocate(self%messages(1)%message, source = message)
            self%length = 1
        else
            self%length = self%length + 1
            old_messages = self%messages
            deallocate(self%messages)
            allocate(self%messages(self%length))
            self%messages(1:size(old_messages)) = old_messages
            allocate(self%messages(self%length)%message, source = message)
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
        type(MessageItem_t) :: old_messages(self%length)
        integer :: num_new_messages
        integer :: num_old_messages
        integer :: total_num_messages

        if (.not. messages%length == 0) then
            if (self%length == 0) then
                allocate(self%messages, source = messages%messages)
                self%length = size(self%messages)
                do i = 1, self%length
                    call self%messages(i)%message%prependNames(module_, procedure_)
                end do
            else
                num_old_messages = self%length
                num_new_messages = messages%length
                total_num_messages = num_old_messages + num_new_messages
                old_messages = self%messages
                deallocate(self%messages)
                allocate(self%messages(total_num_messages))
                self%messages(1:num_old_messages) = old_messages
                self%messages(num_old_messages+1:) = messages%messages
                do i = num_old_messages + 1, total_num_messages
                    call self%messages(i)%message%prependNames(module_, procedure_)
                end do
                self%length = total_num_messages
            end if
        end if
    end subroutine appendMessages

    function ofType(self, type_tag) result(new_list)
        use Message_m, only: MessageType_t

        class(MessageList_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tag
        type(MessageList_t) :: new_list

        new_list = self.ofTypes.[type_tag]
    end function ofType

    function ofTypes(self, type_tags) result(new_list)
        use Message_m, only: MessageType_t

        class(MessageList_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tags(:)
        type(MessageList_t) :: new_list

        logical :: final_mask(self%length)
        integer :: i, j
        logical :: individual_masks(self%length, size(type_tags))
        integer :: num_output
        integer :: num_tags

        if (.not. self%length == 0) then
            num_tags = size(type_tags)
            do i = 1, num_tags
                do j = 1, self%length
                    individual_masks(j, i) = self%messages(j)%message.isType.type_tags(i)
                end do
            end do
            do i = 1, self%length
                final_mask(i) = any(individual_masks(i, :))
            end do
            num_output = count(final_mask)
            if (num_output > 0) then
                allocate(new_list%messages(num_output))
                new_list%length = num_output
                new_list%messages = pack(self%messages, mask=final_mask)
            end if
        end if
    end function ofTypes

    function originatingFromModule(self, module_) result(new_list)
        use Module_m, only: Module_t

        class(MessageList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        type(MessageList_t) :: new_list

        new_list = self.originatingFrom.[module_]
    end function originatingFromModule

    function originatingFromModules(self, modules) result(new_list)
        use Module_m, only: Module_t

        class(MessageList_t), intent(in) :: self
        type(Module_t), intent(in) :: modules(:)
        type(MessageList_t) :: new_list

        logical :: final_mask(self%length)
        integer :: i, j
        logical :: individual_masks(self%length, size(modules))
        integer :: num_modules
        integer :: num_output

        if (.not. self%length == 0) then
            num_modules = size(modules)
            do i = 1, num_modules
                do j = 1, self%length
                    individual_masks(j, i) = self%messages(j)%message.originatedFrom.modules(i)
                end do
            end do
            do i = 1, self%length
                final_mask(i) = any(individual_masks(i, :))
            end do
            num_output = count(final_mask)
            if (num_output > 0) then
                allocate(new_list%messages(num_output))
                new_list%length = num_output
                new_list%messages = pack(self%messages, mask=final_mask)
            end if
        end if
    end function originatingFromModules

    function originatingFromProcedure(self, procedure_) result(new_list)
        use Procedure_m, only: Procedure_t

        class(MessageList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        type(MessageList_t) :: new_list

        new_list = self.originatingFrom.[procedure_]
    end function originatingFromProcedure

    function originatingFromProcedures(self, procedures) result(new_list)
        use Procedure_m, only: Procedure_t

        class(MessageList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedures(:)
        type(MessageList_t) :: new_list

        logical :: final_mask(self%length)
        integer :: i, j
        logical :: individual_masks(self%length, size(procedures))
        integer :: num_output
        integer :: num_procedures

        if (.not. self%length == 0) then
            num_procedures = size(procedures)
            do i = 1, num_procedures
                do j = 1, self%length
                    individual_masks(j, i) = self%messages(j)%message.originatedFrom.procedures(i)
                end do
            end do
            do i = 1, self%length
                final_mask(i) = any(individual_masks(i, :))
            end do
            num_output = count(final_mask)
            if (num_output > 0) then
                allocate(new_list%messages(num_output))
                new_list%length = num_output
                new_list%messages = pack(self%messages, mask=final_mask)
            end if
        end if
    end function originatingFromProcedures

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)
        use strff, only: join, NEWLINE

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING) :: string

        integer :: i
        type(VARYING_STRING) :: strings(self%length)

        if (self%length == 0) then
            string = ""
        else
            do i = 1, self%length
                strings(i) = self%messages(i)%message%toString()
            end do
            string = join(strings, NEWLINE)
        end if
    end function toString

    function messageListSize(self) result(length)
        class(MessageList_t), intent(in) :: self
        integer :: length

        length = self%length
    end function messageListSize
end module Message_list_m
