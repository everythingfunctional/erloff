module Message_list_m
    use Message_m, only: Message_t

    implicit none
    private

    type :: MessageItem_t
        class(Message_t), allocatable :: message
    contains
        procedure :: toString => itemToString
        procedure :: isType
        generic :: operator(.isType.) => isType
        procedure :: originatedFromProcedure
        generic :: operator(.originatedFromProcedure.) => &
                originatedFromProcedure
        procedure :: originatedFromModule
        generic :: operator(.originatedFromModule.) => &
                originatedFromModule
    end type MessageItem_t

    type, public :: MessageList_t
        private
        type(MessageItem_t), allocatable :: messages(:)
    contains
        private
        procedure, public :: appendMessage
        procedure, public :: appendMessages
        procedure :: prependNamesCC
        procedure :: prependNamesCS
        procedure :: prependNamesSC
        procedure :: prependNamesSS
        generic, public :: prependNames => &
                prependNamesCC, prependNamesCS, prependNamesSC, prependNamesSS
        procedure, public :: toString
        procedure :: ofType
        generic, public :: operator(.ofType.) => ofType
        procedure :: ofTypes
        generic, public :: operator(.ofTypes.) => ofTypes
        procedure :: originatingFromProcedureC
        procedure :: originatingFromProcedureS
        generic, public :: operator(.originatingFromProcedure.) => &
                originatingFromProcedureC, originatingFromProcedureS
        procedure :: originatingFromProcedures_
        generic, public :: operator(.originatingFromProcedures.) => &
                originatingFromProcedures_
        procedure :: originatingFromModuleC
        procedure :: originatingFromModuleS
        generic, public :: operator(.originatingFromModule.) => &
                originatingFromModuleC, originatingFromModuleS
        procedure :: originatingFromModules_
        generic, public :: operator(.originatingFromModules.) => &
                originatingFromModules_
    end type MessageList_t

    interface size
        module procedure messageListSize
    end interface size

    public :: size
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

        if (allocated(self%messages)) then
            num_messages = size(messages%messages)
            allocate(messages%messages(num_messages))
            do i = 1, num_messages
                allocate(messages%messages(i)%message, source = &
                        self%messages(i)%message%prependNames( &
                                module_name, procedure_name))
            end do
        end if
    end function prependNamesCC

    pure function prependNamesCS( &
            self, module_name, procedure_name) result(messages)
        use iso_varying_string, only: VARYING_STRING

        class(MessageList_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(MessageList_t) :: messages

        integer :: i
        integer :: num_messages

        if (allocated(self%messages)) then
            num_messages = size(messages%messages)
            allocate(messages%messages(num_messages))
            do i = 1, num_messages
                allocate(messages%messages(i)%message, source = &
                        self%messages(i)%message%prependNames( &
                                module_name, procedure_name))
            end do
        end if
    end function prependNamesCS

    pure function prependNamesSC( &
            self, module_name, procedure_name) result(messages)
        use iso_varying_string, only: VARYING_STRING

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(MessageList_t) :: messages

        integer :: i
        integer :: num_messages

        if (allocated(self%messages)) then
            num_messages = size(messages%messages)
            allocate(messages%messages(num_messages))
            do i = 1, num_messages
                allocate(messages%messages(i)%message, source = &
                        self%messages(i)%message%prependNames( &
                                module_name, procedure_name))
            end do
        end if
    end function prependNamesSC

    pure function prependNamesSS( &
            self, module_name, procedure_name) result(messages)
        use iso_varying_string, only: VARYING_STRING

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(MessageList_t) :: messages

        integer :: i
        integer :: num_messages

        if (allocated(self%messages)) then
            num_messages = size(messages%messages)
            allocate(messages%messages(num_messages))
            do i = 1, num_messages
                allocate(messages%messages(i)%message, source = &
                        self%messages(i)%message%prependNames( &
                                module_name, procedure_name))
            end do
        end if
    end function prependNamesSS

    pure function ofType(self, type_tag) result(new_list)
        use Message_m, only: MessageType_t

        class(MessageList_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tag
        type(MessageList_t) :: new_list

        new_list = self.ofTypes.[type_tag]
    end function ofType

    pure function ofTypes(self, type_tags) result(new_list)
        use Message_m, only: MessageType_t

        class(MessageList_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tags(:)
        type(MessageList_t) :: new_list

        logical :: final_mask(size(self%messages))
        integer :: i
        logical :: individual_masks(size(self%messages), size(type_tags))

        if (allocated(self%messages)) then
            do i = 1, size(type_tags)
                individual_masks(:, i) = self%messages.isType.type_tags(i)
            end do
            do i = 1, size(self%messages)
                final_mask(i) = any(individual_masks(i, :))
            end do
            allocate(new_list%messages(count(final_mask)))
            new_list%messages = pack(self%messages, mask = final_mask)
        end if
    end function ofTypes

    pure function originatingFromProcedureC( &
            self, procedure_name) result(new_list)
        use iso_varying_string, only: var_str

        class(MessageList_t), intent(in) :: self
        character(len=*), intent(in) :: procedure_name
        type(MessageList_t) :: new_list

        new_list = self.originatingFromProcedures.[var_str(procedure_name)]
    end function originatingFromProcedureC

    pure function originatingFromProcedureS( &
            self, procedure_name) result(new_list)
        use iso_varying_string, only: VARYING_STRING

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: procedure_name
        type(MessageList_t) :: new_list

        new_list = self.originatingFromProcedures.[procedure_name]
    end function originatingFromProcedureS

    pure function originatingFromProcedures_( &
            self, procedure_names) result(new_list)
        use iso_varying_string, only: VARYING_STRING

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: procedure_names(:)
        type(MessageList_t) :: new_list

        logical :: final_mask(size(self%messages))
        integer :: i
        logical :: individual_masks(size(self%messages), size(procedure_names))

        if (allocated(self%messages)) then
            do i = 1, size(procedure_names)
                individual_masks(:, i) = &
                        self%messages.originatedFromProcedure.procedure_names(i)
            end do
            do i = 1, size(self%messages)
                final_mask(i) = any(individual_masks(i, :))
            end do
            allocate(new_list%messages(count(final_mask)))
            new_list%messages = pack(self%messages, mask = final_mask)
        end if
    end function originatingFromProcedures_

    pure function originatingFromModuleC(self, module_name) result(new_list)
        use iso_varying_string, only: var_str

        class(MessageList_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        type(MessageList_t) :: new_list

        new_list = self.originatingFromModules.[var_str(module_name)]
    end function originatingFromModuleC

    pure function originatingFromModuleS(self, module_name) result(new_list)
        use iso_varying_string, only: VARYING_STRING

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        type(MessageList_t) :: new_list

        new_list = self.originatingFromModules.[module_name]
    end function originatingFromModuleS

    pure function originatingFromModules_(self, module_names) result(new_list)
        use iso_varying_string, only: VARYING_STRING

        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_names(:)
        type(MessageList_t) :: new_list

        logical :: final_mask(size(self%messages))
        integer :: i
        logical :: individual_masks(size(self%messages), size(module_names))

        if (allocated(self%messages)) then
            do i = 1, size(module_names)
                individual_masks(:, i) = &
                        self%messages.originatedFromModule.module_names(i)
            end do
            do i = 1, size(self%messages)
                final_mask(i) = any(individual_masks(i, :))
            end do
            allocate(new_list%messages(count(final_mask)))
            new_list%messages = pack(self%messages, mask = final_mask)
        end if
    end function originatingFromModules_

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

    elemental function isType(self, type_tag)
        use Message_m, only: MessageType_t

        class(MessageItem_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tag
        logical :: isType

        isType = self%message.isType.type_tag
    end function isType

    elemental function originatedFromProcedure(self, procedure_name)
        use iso_varying_string, only: VARYING_STRING

        class(MessageItem_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: procedure_name
        logical :: originatedFromProcedure

        originatedFromProcedure = &
                self%message.originatedFromProcedure.procedure_name
    end function originatedFromProcedure

    elemental function originatedFromModule(self, module_name)
        use iso_varying_string, only: VARYING_STRING

        class(MessageItem_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        logical :: originatedFromModule

        originatedFromModule = &
                self%message.originatedFromModule.module_name
    end function originatedFromModule

    elemental function itemToString(self) result(string)
        use iso_varying_string, only: VARYING_STRING

        class(MessageItem_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%message%toString()
    end function itemToString

    pure function messageListSize(list) result(num_messages)
        type(MessageList_t), intent(in) :: list
        integer :: num_messages

        if (allocated(list%messages)) then
            num_messages = size(list%messages)
        else
            num_messages = 0
        end if
    end function messageListSize
end module Message_list_m
