module Call_stack_entry_m
    use Module_m, only: Module_t
    use Procedure_m, only: Procedure_t

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
    function CallStackEntry(module_, procedure_) result(entry_)
        use Module_m, only: Module_t
        use Procedure_m, only: Procedure_t

        type(Module_t), intent(in) :: module_
        type(Procedure_t), intent(in) :: procedure_
        type(CallStackEntry_t) :: entry_

        entry_%module_ = module_
        entry_%procedure_ = procedure_
    end function CallStackEntry

    function toString(self) result(string)
        use iso_varying_string, only: VARYING_STRING, operator(//)

        class(CallStackEntry_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%module_%toString() // "." // self%procedure_%toString()
    end function toString

    function isFromModule(self, module_)
        use Module_m, only: Module_t

        class(CallStackEntry_t), intent(in) :: self
        type(Module_t), intent(in) :: module_
        logical :: isFromModule

        isFromModule = self%module_ == module_
    end function isFromModule

    function isFromProcedure(self, procedure_)
        use Procedure_m, only: Procedure_t

        class(CallStackEntry_t), intent(in) :: self
        type(Procedure_t), intent(in) :: procedure_
        logical :: isFromProcedure

        isFromProcedure = self%procedure_ == procedure_
    end function isFromProcedure

    function repr(self)
        use iso_varying_string, only: VARYING_STRING, operator(//)
        use strff, only: hangingIndent, indent, NEWLINE

        class(CallStackEntry_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = hangingIndent( &
                "CallStackEntry_t(" // NEWLINE &
                    // "module = " // self%module_%repr() // "," // NEWLINE &
                    // "procedure = " // self%procedure_%repr(), &
                4) // NEWLINE // ")"
    end function repr
end module Call_stack_entry_m
