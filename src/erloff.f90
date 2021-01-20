module erloff
    use Error_list_m, only: ErrorList_t, size
    use Message_m, only: &
            ! Message types
            INPUTS_TYPE, &
            NOT_FOUND_TYPE, &
            OUT_OF_BOUNDS_TYPE, &
            OUTPUTS_TYPE, &
            OUTSIDE_NORMAL_RANGE_TYPE, &
            UNEQUAL_ARRAY_SIZES_TYPE, &
            UNKNOWN_TYPE_TYPE
    use Message_list_m, only: MessageList_t, size
    use erloff_debug_m, only: debug_t, DEBUG
    use erloff_debug_level_m, only: GENERAL, MEDIUM, DETAILED, NITTY_GRITTY
    use erloff_error_m, only: error_t, ERROR
    use erloff_fatal_m, only: fatal_t, FATAL
    use erloff_info_m, only: info_t, INFO
    use erloff_internal_m, only: internal_t, INTERNAL
    use erloff_message_m, only: message_t
    use erloff_message_type_m, only: message_type_t
    use erloff_module_m, only: module_t
    use erloff_procedure_m, only: procedure_t
    use erloff_warning_m, only: warning_t, WARNING
end module
