module Module_m
    use iso_varying_string, only: &
            VARYING_STRING, operator(//), operator(==), var_str

    implicit none
    private

    type, public :: Module_t
        private
        type(VARYING_STRING) :: name
    contains
        private
        procedure :: moduleEquals
        generic, public :: operator(==) => moduleEquals
        procedure, public :: toString
        procedure, public :: repr
    end type Module_t

    interface Module_
        module procedure ModuleC
        module procedure ModuleS
    end interface Module_

    public :: Module_
contains
    pure function ModuleC(name)
        character(len=*), intent(in) :: name
        type(Module_t) :: ModuleC

        ModuleC = Module_(var_str(name))
    end function ModuleC

    pure function ModuleS(name)
        type(VARYING_STRING), intent(in) :: name
        type(Module_t) :: ModuleS

        ModuleS%name = name
    end function ModuleS

    pure function moduleEquals(lhs, rhs)
        class(Module_t), intent(in) :: lhs
        type(Module_t), intent(in) :: rhs
        logical :: moduleEquals

        moduleEquals = lhs%name == rhs%name
    end function moduleEquals

    pure function toString(self) result(string)
        class(Module_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%name
    end function toString

    pure function repr(self)
        class(Module_t), intent(in) :: self
        type(VARYING_STRING) :: repr

        repr = 'Module_t("' // self%name // '")'
    end function repr
end module Module_m
