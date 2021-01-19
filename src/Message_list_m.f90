module Message_list_m
    use iso_varying_string, only: &
            VARYING_STRING, assignment(=), operator(//), var_str
    use Message_m, only: Message_t, MessageType_t
    use Module_m, only: Module_t
    use Procedure_m, only: Procedure_t
    use strff, only: hanging_indent, indent, join, NEWLINE

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
        procedure :: comingThroughModule
        procedure :: comingThroughModules
        procedure :: comingThroughProcedure
        procedure :: comingThroughProcedures
        generic, public :: operator(.comingThrough.) => &
                comingThroughModule, &
                comingThroughModules, &
                comingThroughProcedure, &
                comingThroughProcedures
        procedure :: fromModule
        procedure :: fromModules
        procedure :: fromProcedure
        procedure :: fromProcedures
        generic, public :: operator(.from.) => &
                fromModule, &
                fromModules, &
                fromProcedure, &
                fromProcedures
        procedure :: includingC
        procedure :: includingS
        generic, public :: operator(.including.) => includingC, includingS
        procedure :: includingAnyOf
        generic, public :: operator(.includingAnyOf.) => includingAnyOf
        procedure :: includingAllOf
        generic, public :: operator(.includingAllOf.) => includingAllOf
        procedure :: hasType
        generic, public :: operator(.hasType.) => hasType
        procedure :: hasAnyOriginatingFromModule
        procedure :: hasAnyOriginatingFromProcedure
        generic, public :: operator(.hasAnyOriginatingFrom.) => &
                hasAnyOriginatingFromModule, hasAnyOriginatingFromProcedure
        procedure :: hasAnyComingThroughModule
        procedure :: hasAnyComingThroughProcedure
        generic, public :: operator(.hasAnyComingThrough.) => &
                hasAnyComingThroughModule, hasAnyComingThroughProcedure
        procedure :: hasAnyFromModule
        procedure :: hasAnyFromProcedure
        generic, public :: operator(.hasAnyFrom.) => &
                hasAnyFromModule, hasAnyFromProcedure
        procedure :: hasAnyIncludingC
        procedure :: hasAnyIncludingS
        generic, public :: operator(.hasAnyIncluding.) => &
                hasAnyIncludingC, hasAnyIncludingS
        procedure :: hasAnyIncludingAnyOf
        generic, public :: operator(.hasAnyIncludingAnyOf.) => &
                hasAnyIncludingAnyOf
        procedure :: hasAnyIncludingAllOf
        generic, public :: operator(.hasAnyIncludingAllOf.) => &
                hasAnyIncludingAllOf
        procedure, public :: toString
        procedure, public :: repr
    end type MessageList_t

    interface size
        module procedure messageListSize
    end interface size

    public :: size
contains
    pure subroutine appendMessage(self, message)
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

    pure subroutine appendMessages(self, messages, module_, procedure_)
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

    pure function ofType(self, type_tag) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tag
        type(MessageList_t) :: new_list

        new_list = self.ofTypes.[type_tag]
    end function ofType

    pure function ofTypes(self, type_tags) result(new_list)
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

    pure function originatingFromModule(self, module_) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        type(MessageList_t) :: new_list

        new_list = self.originatingFrom.[module_]
    end function originatingFromModule

    pure function originatingFromModules(self, modules) result(new_list)
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

    pure function originatingFromProcedure(self, procedure_) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        type(MessageList_t) :: new_list

        new_list = self.originatingFrom.[procedure_]
    end function originatingFromProcedure

    pure function originatingFromProcedures(self, procedures) result(new_list)
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

    pure function comingThroughModule(self, module_) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        type(MessageList_t) :: new_list

        new_list = self.comingThrough.[module_]
    end function comingThroughModule

    pure function comingThroughModules(self, modules) result(new_list)
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
                    individual_masks(j, i) = self%messages(j)%message.cameThrough.modules(i)
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
    end function comingThroughModules

    pure function comingThroughProcedure(self, procedure_) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        type(MessageList_t) :: new_list

        new_list = self.comingThrough.[procedure_]
    end function comingThroughProcedure

    pure function comingThroughProcedures(self, procedures) result(new_list)
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
                    individual_masks(j, i) = self%messages(j)%message.cameThrough.procedures(i)
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
    end function comingThroughProcedures

    pure function fromModule(self, module_) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        type(MessageList_t) :: new_list

        new_list = self.from.[module_]
    end function fromModule

    pure function fromModules(self, modules) result(new_list)
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
                    individual_masks(j, i) = self%messages(j)%message.isFrom.modules(i)
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
    end function fromModules

    pure function fromProcedure(self, procedure_) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        type(MessageList_t) :: new_list

        new_list = self.from.[procedure_]
    end function fromProcedure

    pure function fromProcedures(self, procedures) result(new_list)
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
                    individual_masks(j, i) = self%messages(j)%message.isFrom.procedures(i)
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
    end function fromProcedures

    pure function includingC(self, string) result(new_list)
        class(MessageList_t), intent(in) :: self
        character(len=*), intent(in) :: string
        type(MessageList_t) :: new_list

        new_list = self.including.var_str(string)
    end function includingC

    pure function includingS(self, string) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        type(MessageList_t) :: new_list

        new_list = self.includingAnyOf.[string]
    end function includingS

    pure function includingAnyOf(self, strings) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        type(MessageList_t) :: new_list

        integer :: i
        logical :: mask(self%length)
        integer :: num_output

        if (.not. self%length == 0) then
            do i = 1, self%length
                mask(i) = self%messages(i)%message.includesAnyOf.strings
            end do
            num_output = count(mask)
            if (num_output > 0) then
                allocate(new_list%messages(num_output))
                new_list%messages = pack(self%messages, mask = mask)
                new_list%length = num_output
            end if
        end if
    end function includingAnyOf

    pure function includingAllOf(self, strings) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        type(MessageList_t) :: new_list

        integer :: i
        logical :: mask(self%length)
        integer :: num_output

        if (.not. self%length == 0) then
            do i = 1, self%length
                mask(i) = self%messages(i)%message.includesAllOf.strings
            end do
            num_output = count(mask)
            if (num_output > 0) then
                allocate(new_list%messages(num_output))
                new_list%messages = pack(self%messages, mask = mask)
                new_list%length = num_output
            end if
        end if
    end function includingAllOf

    pure function hasType(self, type_tag)
        class(MessageList_t), intent(in) :: self
        type(MessageType_t), intent(in) :: type_tag
        logical :: hasType

        hasType = size(self.ofType.type_tag) > 0
    end function hasType

    pure function hasAnyOriginatingFromModule(self, module_) result(has_any)
        class(MessageList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: has_any

        has_any = size(self.originatingFrom.module_) > 0
    end function hasAnyOriginatingFromModule

    pure function hasAnyOriginatingFromProcedure(self, procedure_) result(has_any)
        class(MessageList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: has_any

        has_any = size(self.originatingFrom.procedure_) > 0
    end function hasAnyOriginatingFromProcedure

    pure function hasAnyComingThroughModule(self, module_) result(has_any)
        class(MessageList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: has_any

        has_any = size(self.comingThrough.module_) > 0
    end function hasAnyComingThroughModule

    pure function hasAnyComingThroughProcedure(self, procedure_) result(has_any)
        class(MessageList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: has_any

        has_any = size(self.comingThrough.procedure_) > 0
    end function hasAnyComingThroughProcedure

    pure function hasAnyFromModule(self, module_) result(has_any)
        class(MessageList_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: has_any

        has_any = size(self.from.module_) > 0
    end function hasAnyFromModule

    pure function hasAnyFromProcedure(self, procedure_) result(has_any)
        class(MessageList_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: has_any

        has_any = size(self.from.procedure_) > 0
    end function hasAnyFromProcedure

    pure function hasAnyIncludingC(self, string) result(has_any)
        class(MessageList_t), intent(in) :: self
        character(len=*), intent(in) :: string
        logical :: has_any

        has_any = size(self.including.string) > 0
    end function hasAnyIncludingC

    pure function hasAnyIncludingS(self, string) result(has_any)
        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: string
        logical :: has_any

        has_any = size(self.including.string) > 0
    end function hasAnyIncludingS

    pure function hasAnyIncludingAnyOf(self, strings) result(has_any)
        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        logical :: has_any

        has_any = size(self.includingAnyOf.strings) > 0
    end function hasAnyIncludingAnyOf

    pure function hasAnyIncludingAllOf(self, strings) result(has_any)
        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        logical :: has_any

        has_any = size(self.includingAllOf.strings) > 0
    end function hasAnyIncludingAllOf

    pure function toString(self) result(string)
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

    pure function repr(self)
        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        integer :: i
        type(VARYING_STRING) :: strings(self%length)

        do i = 1, self%length
            strings(i) = self%messages(i)%message%repr()
        end do

        repr = hanging_indent( &
                "MessageList_t(" // NEWLINE &
                    // "messages = [" // NEWLINE &
                    // indent(join(strings, "," // NEWLINE), 4) // NEWLINE // "]", &
                4) // NEWLINE // ")"
    end function repr

    pure function messageListSize(self) result(length)
        class(MessageList_t), intent(in) :: self
        integer :: length

        length = self%length
    end function messageListSize
end module Message_list_m
