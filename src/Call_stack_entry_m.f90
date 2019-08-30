module Call_stack_entry_m
    use iso_varying_string, only: VARYING_STRING

    implicit none
    private

    type, public :: CallStackEntry_t
        private
        type(VARYING_STRING) :: module_name
        type(VARYING_STRING) :: procedure_name
    contains
        private
        procedure, public :: toString
        procedure :: isFromModuleC
        procedure :: isFromModuleS
        generic, public :: operator(.isFromModule.) => &
                isFromModuleC, isFromModuleS
        procedure :: isFromProcedureC
        procedure :: isFromProcedureS
        generic, public :: operator(.isFromProcedure.) => &
                isFromProcedureC, isFromProcedureS
        procedure, public :: repr
    end type CallStackEntry_t

    interface CallStackEntry
        module procedure CallStackEntryCC
        module procedure CallStackEntryCS
        module procedure CallStackEntrySC
        module procedure CallStackEntrySS
    end interface CallStackEntry

    public :: CallStackEntry
contains
    pure function CallStackEntryCC(module_name, procedure_name) result(entry_)
        use iso_varying_string, only: assignment(=)

        character(len=*), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(CallStackEntry_t) :: entry_

        entry_%module_name = module_name
        entry_%procedure_name = procedure_name
    end function CallStackEntryCC

    pure function CallStackEntryCS(module_name, procedure_name) result(entry_)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        character(len=*), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(CallStackEntry_t) :: entry_

        entry_%module_name = module_name
        entry_%procedure_name = procedure_name
    end function CallStackEntryCS

    pure function CallStackEntrySC(module_name, procedure_name) result(entry_)
        use iso_varying_string, only: VARYING_STRING, assignment(=)

        type(VARYING_STRING), intent(in) :: module_name
        character(len=*), intent(in) :: procedure_name
        type(CallStackEntry_t) :: entry_

        entry_%module_name = module_name
        entry_%procedure_name = procedure_name
    end function CallStackEntrySC

    pure function CallStackEntrySS(module_name, procedure_name) result(entry_)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: module_name
        type(VARYING_STRING), intent(in) :: procedure_name
        type(CallStackEntry_t) :: entry_

        entry_%module_name = module_name
        entry_%procedure_name = procedure_name
    end function CallStackEntrySS

    elemental function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        class(CallStackEntry_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%module_name // "." // self%procedure_name
    end function toString

    elemental function isFromModuleC(self, module_name)
        use iso_varying_string, only: operator(==)

        class(CallStackEntry_t), intent(in) :: self
        character(len=*), intent(in) :: module_name
        logical :: isFromModuleC

        isFromModuleC = self%module_name == module_name
    end function isFromModuleC

    elemental function isFromModuleS(self, module_name)
        use iso_varying_string, only: VARYING_STRING, operator(==)

        class(CallStackEntry_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: module_name
        logical :: isFromModuleS

        isFromModuleS = self%module_name == module_name
    end function isFromModuleS

    elemental function isFromProcedureC(self, procedure_name)
        use iso_varying_string, only: operator(==)

        class(CallStackEntry_t), intent(in) :: self
        character(len=*), intent(in) :: procedure_name
        logical :: isFromProcedureC

        isFromProcedureC = self%procedure_name == procedure_name
    end function isFromProcedureC

    elemental function isFromProcedureS(self, procedure_name)
        use iso_varying_string, only: VARYING_STRING, operator(==)

        class(CallStackEntry_t), intent(in) :: self
        type(VARYING_STRING), intent(in) :: procedure_name
        logical :: isFromProcedureS

        isFromProcedureS = self%procedure_name == procedure_name
    end function isFromProcedureS

    elemental function repr(self)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: NEWLINE

        class(CallStackEntry_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = &
                'CallStackEntry(' // NEWLINE &
                // '    module_name = "' // self%module_name // '", ' // NEWLINE &
                // '    procedure_name = "' // self%procedure_name // '")'
    end function repr
end module Call_stack_entry_m
