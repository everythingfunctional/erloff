module Call_stack_entry_m
    use iso_varying_string, only: VARYING_STRING, operator(//)
    use Module_m, only: Module_t
    use Procedure_m, only: Procedure_t
    use strff, only: hangingIndent, indent, NEWLINE

    implicit none
    private

    type, public :: CallStackEntry_t
        private
        type(Module_t) :: module_
        type(Procedure_t) :: procedure_
    contains
        private
        procedure, public :: toString
        procedure :: isFromModule
        procedure :: isFromProcedure
        generic, public :: operator(.isFrom.) => &
                isFromModule, isFromProcedure
        procedure, public :: repr
    end type CallStackEntry_t

    public :: CallStackEntry
contains
    pure function CallStackEntry(module_, procedure_) result(entry_)
        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(CallStackEntry_t) :: entry_

        entry_%module_ = module_
        entry_%procedure_ = procedure_
    end function CallStackEntry

    elemental function toString(self) result(string)
        class(CallStackEntry_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%module_%toString() // "." // self%procedure_%toString()
    end function toString

    elemental function isFromModule(self, module_)
        class(CallStackEntry_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: isFromModule

        isFromModule = self%module_ == module_
    end function isFromModule

    elemental function isFromProcedure(self, procedure_)
        class(CallStackEntry_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: isFromProcedure

        isFromProcedure = self%procedure_ == procedure_
    end function isFromProcedure

    elemental function repr(self)
        class(CallStackEntry_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = hangingIndent( &
                "CallStackEntry_t(" // NEWLINE &
                    // "module = " // self%module_%repr() // "," // NEWLINE &
                    // "procedure = " // self%procedure_%repr(), &
                4) // NEWLINE // ")"
    end function repr
end module Call_stack_entry_m
