module Message_list_m
    use erloff_message_m, only: Message_t
    use erloff_message_item_m, only: message_item_t
    use erloff_message_type_m, only: message_type_t
    use iso_varying_string, only: &
            VARYING_STRING, assignment(=), operator(//), var_str
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: Procedure_t
    use strff, only: hanging_indent, indent, join, NEWLINE

    implicit none
    private

    type, public :: MessageList_t
        private
        integer :: length = 0
        type(message_item_t), allocatable :: messages(:)
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
    subroutine appendMessage(self, message)
        class(MessageList_t), intent(inout) :: self
        class(Message_t), intent(in) :: message

        if (self%length == 0) then
            allocate(self%messages, source = [message_item_t(message)])
            self%length = 1
        else
            self%length = self%length + 1
            self%messages = [self%messages, message_item_t(message)]
        end if
    end subroutine appendMessage

    subroutine appendMessages(self, messages, module_, procedure_)
        class(MessageList_t), intent(inout) :: self
        type(MessageList_t), intent(in) :: messages
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_

        if (.not. messages%length == 0) then
            if (self%length == 0) then
                allocate(self%messages, source = &
                        messages%messages%with_names_prepended(module_, procedure_))
            else
                self%messages = &
                        [ self%messages &
                        , messages%messages%with_names_prepended(module_, procedure_) &
                        ]
            end if
            self%length = size(self%messages)
        end if
    end subroutine appendMessages

    pure function ofType(self, type_tag) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
        type(MessageList_t) :: new_list

        new_list = self.ofTypes.[type_tag]
    end function ofType

    pure function ofTypes(self, type_tags) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tags(:)
        type(MessageList_t) :: new_list

        integer :: i

        if (.not. self%length == 0) then
            allocate(new_list%messages, source = pack( &
                    self%messages, &
                    mask = [(any(self%messages(i).isType.type_tags), i = 1, self%length)]))
            new_list%length = size(new_list%messages)
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

        integer :: i

        if (.not. self%length == 0) then
            allocate(new_list%messages, source = pack( &
                    self%messages, &
                    mask = [(any(self%messages(i).originatedFrom.modules), i = 1, self%length)]))
            new_list%length = size(new_list%messages)
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

        integer :: i

        if (.not. self%length == 0) then
            allocate(new_list%messages, source = pack( &
                    self%messages, &
                    mask = [(any(self%messages(i).originatedFrom.procedures), i = 1, self%length)]))
            new_list%length = size(new_list%messages)
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

        integer :: i

        if (.not. self%length == 0) then
            allocate(new_list%messages, source = pack( &
                    self%messages, &
                    mask = [(any(self%messages(i).cameThrough.modules), i = 1, self%length)]))
            new_list%length = size(new_list%messages)
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

        integer :: i

        if (.not. self%length == 0) then
            allocate(new_list%messages, source = pack( &
                    self%messages, &
                    mask = [(any(self%messages(i).cameThrough.procedures), i = 1, self%length)]))
            new_list%length = size(new_list%messages)
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

        integer :: i

        if (.not. self%length == 0) then
            allocate(new_list%messages, source = pack( &
                    self%messages, &
                    mask = [(any(self%messages(i).isFrom.modules), i = 1, self%length)]))
            new_list%length = size(new_list%messages)
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

        integer :: i

        if (.not. self%length == 0) then
            allocate(new_list%messages, source = pack( &
                    self%messages, &
                    mask = [(any(self%messages(i).isFrom.procedures), i = 1, self%length)]))
            new_list%length = size(new_list%messages)
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

        if (.not. self%length == 0) then
            allocate(new_list%messages, source = pack( &
                    self%messages, &
                    mask = [(self%messages(i).includesAnyOf.strings, i = 1, self%length)]))
            new_list%length = size(new_list%messages)
        end if
    end function includingAnyOf

    pure function includingAllOf(self, strings) result(new_list)
        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: strings(:)
        type(MessageList_t) :: new_list

        integer :: i

        if (.not. self%length == 0) then
            allocate(new_list%messages, source = pack( &
                    self%messages, &
                    mask = [(self%messages(i).includesAllOf.strings, i = 1, self%length)]))
            new_list%length = size(new_list%messages)
        end if
    end function includingAllOf

    pure function hasType(self, type_tag)
        class(MessageList_t), intent(in) :: self
        type(message_type_t), intent(in) :: type_tag
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

        if (self%length == 0) then
            string = ""
        else
            string = join(self%messages%to_string(), NEWLINE)
        end if
    end function toString

    pure function repr(self)
        class(MessageList_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        type(varying_string) :: messages_string

        if (self%length == 0) then
            messages_string = "[]"
        else
            messages_string = "[" // NEWLINE &
                    // indent( &
                            join(self%messages%repr(), "," // NEWLINE), &
                            4) // NEWLINE // "]"
        end if

        repr = hanging_indent( &
                "MessageList_t(" // NEWLINE &
                    // "messages = " // messages_string, &
                4) // NEWLINE // ")"
    end function repr

    pure function messageListSize(self) result(length)
        class(MessageList_t), intent(in) :: self
        integer :: length

        length = self%length
    end function messageListSize
end module Message_list_m
