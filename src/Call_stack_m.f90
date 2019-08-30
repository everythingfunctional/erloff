module Call_stack_m
    use Call_stack_entry_m, only: CallStackEntry_t

    implicit none
    private

    type, public :: CallStack_t
        private
        type(CallStackEntry_t), allocatable :: entries(:)
    contains
        private
        procedure :: prependNamesCC
        procedure :: prependNamesCS
        procedure :: prependNamesSC
        procedure :: prependNamesSS
        generic, public :: prependNames => &
                prependNamesCC, prependNamesCS, prependNamesSC, prependNamesSS
        procedure, public :: toString
        procedure :: originatingModuleIsC
        procedure :: originatingModuleIsS
        generic, public :: operator(.originatingModuleIs.) => &
                originatingModuleIsC, originatingModuleIsS
        procedure :: originatingProcedureIsC
        procedure :: originatingProcedureIsS
        generic, public :: operator(.originatingProcedureIs.) => &
                originatingProcedureIsC, originatingProcedureIsS
        procedure :: containsModuleC
        procedure :: containsModuleS
        generic, public :: operator(.containsModule.) => &
                containsModuleC, containsModuleS
        procedure :: containsProcedureC
        procedure :: containsProcedureS
        generic, public :: operator(.containsProcedure.) => &
                containsProcedureC, containsProcedureS
        procedure, public :: repr
    end type CallStack_t

    interface CallStack
        module procedure CallStackCC
        module procedure CallStackCS
        module procedure CallStackSC
        module procedure CallStackSS
    end interface CallStack

    public :: CallStack
contains
    pure function CallStackCC(module_name, procedure_name) result(new_stack)
        use Call_stack_entry_m, only: CallStackEntry

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(CallStack_t) :: new_stack

        allocate(new_stack%entries(1))
        new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
    end function CallStackCC

    pure function CallStackCS(module_name, procedure_name) result(new_stack)
        use Call_stack_entry_m, only: CallStackEntry
        use iso_varying_string, only: VARYING_STRING

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(CallStack_t) :: new_stack

        allocate(new_stack%entries(1))
        new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
    end function CallStackCS

    pure function CallStackSC(module_name, procedure_name) result(new_stack)
        use Call_stack_entry_m, only: CallStackEntry
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(CallStack_t) :: new_stack

        allocate(new_stack%entries(1))
        new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
    end function CallStackSC

    pure function CallStackSS(module_name, procedure_name) result(new_stack)
        use Call_stack_entry_m, only: CallStackEntry
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(CallStack_t) :: new_stack

        allocate(new_stack%entries(1))
        new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
    end function CallStackSS

    pure function prependNamesCC( &
            self, module_name, procedure_name) result(new_stack)
        use Call_stack_entry_m, only: CallStackEntry

        class(CallStack_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(CallStack_t) :: new_stack

        integer :: new_stack_length

        if (allocated(self%entries)) then
            new_stack_length = size(self%entries) + 1
            allocate(new_stack%entries(new_stack_length))
            new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
            new_stack%entries(2:new_stack_length) = self%entries(:)
        else
            allocate(new_stack%entries(1))
            new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
        end if
    end function prependNamesCC

    pure function prependNamesCS( &
            self, module_name, procedure_name) result(new_stack)
        use Call_stack_entry_m, only: CallStackEntry
        use iso_varying_string, only: VARYING_STRING

        class(CallStack_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(CallStack_t) :: new_stack

        integer :: new_stack_length

        if (allocated(self%entries)) then
            new_stack_length = size(self%entries) + 1
            allocate(new_stack%entries(new_stack_length))
            new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
            new_stack%entries(2:new_stack_length) = self%entries(:)
        else
            allocate(new_stack%entries(1))
            new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
        end if
    end function prependNamesCS

    pure function prependNamesSC( &
            self, module_name, procedure_name) result(new_stack)
        use Call_stack_entry_m, only: CallStackEntry
        use iso_varying_string, only: VARYING_STRING

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(CallStack_t) :: new_stack

        integer :: new_stack_length

        if (allocated(self%entries)) then
            new_stack_length = size(self%entries) + 1
            allocate(new_stack%entries(new_stack_length))
            new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
            new_stack%entries(2:new_stack_length) = self%entries(:)
        else
            allocate(new_stack%entries(1))
            new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
        end if
    end function prependNamesSC

    pure function prependNamesSS( &
            self, module_name, procedure_name) result(new_stack)
        use Call_stack_entry_m, only: CallStackEntry
        use iso_varying_string, only: VARYING_STRING

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(CallStack_t) :: new_stack

        integer :: new_stack_length

        if (allocated(self%entries)) then
            new_stack_length = size(self%entries) + 1
            allocate(new_stack%entries(new_stack_length))
            new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
            new_stack%entries(2:new_stack_length) = self%entries(:)
        else
            allocate(new_stack%entries(1))
            new_stack%entries(1) = CallStackEntry(module_name, procedure_name)
        end if
    end function prependNamesSS

    pure function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, assignment(=)
        use strff, only: join

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING) :: string

        if (allocated(self%entries)) then
            string = join(self%entries%toString(), "->")
        else
            string = ""
        end if
    end function toString

    pure function originatingModuleIsC(self, module_name)
        class(CallStack_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        logical :: originatingModuleIsC

        if (allocated(self%entries)) then
            originatingModuleIsC = &
                    self%entries(size(self%entries)).isFromModule.module_name
        else
            originatingModuleIsC = .false.
        end if
    end function originatingModuleIsC

    pure function originatingModuleIsS(self, module_name)
        use iso_varying_string, only: VARYING_STRING

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        logical :: originatingModuleIsS

        if (allocated(self%entries)) then
            originatingModuleIsS = &
                    self%entries(size(self%entries)).isFromModule.module_name
        else
            originatingModuleIsS = .false.
        end if
    end function originatingModuleIsS

    pure function originatingProcedureIsC(self, procedure_name)
        class(CallStack_t), intent(in) :: self
        character(len=*), intent(in) :: procedure_name
        logical :: originatingProcedureIsC

        if (allocated(self%entries)) then
            originatingProcedureIsC = &
                    self%entries(size(self%entries)).isFromProcedure.procedure_name
        else
            originatingProcedureIsC = .false.
        end if
    end function originatingProcedureIsC

    pure function originatingProcedureIsS(self, procedure_name)
        use iso_varying_string, only: VARYING_STRING

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: procedure_name
        logical :: originatingProcedureIsS

        if (allocated(self%entries)) then
            originatingProcedureIsS = &
                    self%entries(size(self%entries)).isFromProcedure.procedure_name
        else
            originatingProcedureIsS = .false.
        end if
    end function originatingProcedureIsS

    pure function containsModuleC(self, module_name)
        class(CallStack_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        logical :: containsModuleC

        if (allocated(self%entries)) then
            containsModuleC = any(self%entries.isFromModule.module_name)
        else
            containsModuleC = .false.
        end if
    end function containsModuleC

    pure function containsModuleS(self, module_name)
        use iso_varying_string, only: VARYING_STRING

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        logical :: containsModuleS

        if (allocated(self%entries)) then
            containsModuleS = any(self%entries.isFromModule.module_name)
        else
            containsModuleS = .false.
        end if
    end function containsModuleS

    pure function containsProcedureC(self, procedure_name)
        class(CallStack_t), intent(in) :: self
        character(len=*), intent(in) :: procedure_name
        logical :: containsProcedureC

        if (allocated(self%entries)) then
            containsProcedureC = any(self%entries.isFromProcedure.procedure_name)
        else
            containsProcedureC = .false.
        end if
    end function containsProcedureC

    pure function containsProcedureS(self, procedure_name)
        use iso_varying_string, only: VARYING_STRING

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: procedure_name
        logical :: containsProcedureS

        if (allocated(self%entries)) then
            containsProcedureS = any(self%entries.isFromProcedure.procedure_name)
        else
            containsProcedureS = .false.
        end if
    end function containsProcedureS

    pure function repr(self)
        use iso_varying_string, only: &
                VARYING_STRING, assignment(=), operator(//)
        use strff, only: hangingIndent, join, NEWLINE

        class(CallStack_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        if (allocated(self%entries)) then
            repr = hangingIndent( &
                    "CallStack([" // NEWLINE &
                    // join(self%entries%repr(), "," // NEWLINE) &
                    // "])", &
                    4)
        else
            repr = "CallStack([])"
        end if
    end function repr
end module Call_stack_m
