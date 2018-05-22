// NOTE: element #1 is the error code and element #2 is a short description of the error case.
// The description isn't used by the compiler itself but is there for tools to utilize
// in the future.
#define ERROR_CODES \
    FOR_EACH(InvalidEscape, "Escape sequence is an invalid Unicode codepoint"), \
    FOR_EACH(InvalidCharacterEscape, "Escape sequence is a invalid"), \
    FOR_EACH(InvalidNumericEscape, "Escape sequence is an invalid Unicode codepoint"), \
    FOR_EACH(InvalidCodePoint, "An invalid Unicode codepoint"), \
    FOR_EACH(StringContainsNewline, "A string literal contains a newline"), \
    FOR_EACH(UnexpectedEOF, "An unexpected EOF while parsing a string literal"), \
    FOR_EACH(ExpectedDigit, "Expected a digit"), \
    FOR_EACH(DigitOutOfRange, "Digit is out of range of base"), \
    FOR_EACH(FloatOverflow, "Float literal overflow"), \
    FOR_EACH(IntOverflow, "Integer literal overflow"), \
    FOR_EACH(WrongDoubleQuote, "User entered `“` (0x201c) as a quote instead of ASCII"), \
    FOR_EACH(Syntax, "Syntax error"), \
    FOR_EACH(Fatal, "Fatal error"), \

typedef enum ErrorCode {
#define FOR_EACH(e, s) e##Error
    ERROR_CODES
#undef FOR_EACH
} ErrorCode;

typedef struct Error {
    ErrorCode code;
    Position pos;
    const char *message;
} Error;

typedef struct ErrorCollector {
    u32 errorCount;
} ErrorCollector;

ErrorCollector errorCollector;

b32 HasErrors() {
    return errorCollector.errorCount > 0;
}

#define tempErrorBufferLen KB(4)
char *temporaryErrorBuffer;

void InitErrorBuffers() {
    temporaryErrorBuffer = malloc(tempErrorBufferLen);
};

// NOTE: this function is not threadsafe
char *errorBuffPrintf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    // NOTE: vsnprintf will always null-terminate the buffer so we don't need to
    // pass around the length
    vsnprintf(temporaryErrorBuffer, tempErrorBufferLen, fmt, args);

    va_end(args);

    return temporaryErrorBuffer;
}
 
void Report(Error error) {
#ifndef NO_ERROR_CODES
    if (FlagErrorCodes) {
        fprintf(
            stderr, 
            "ERROR(%s:%u:%u, E%04d): %s\n",
            error.pos.name, 
            error.pos.line, 
            error.pos.column, 
            error.code, 
            error.message
        );
    } else {
#endif

#ifndef NO_ERROR_CODES
        fprintf(
            stderr, 
            "ERROR(%s:%u:%u): %s\n",
            error.pos.name, 
            error.pos.line, 
            error.pos.column, 
            error.message
        );
    }
#endif

    errorCollector.errorCount += 1;
}

b32 shouldPrintErrorCode() {
#if NO_ERROR_CODES
    return false;
#endif
    return FlagErrorCodes;
}

void ReportError(ErrorCode code, Position pos, const char *msg, ...) {
    va_list args;
    char buf[1024];
    va_start(args, msg);
    vsnprintf(buf, sizeof(buf), msg, args);
    if (shouldPrintErrorCode()) {
        fprintf(stderr, "ERROR(%s:%u:%u, E%04d): %s\n", pos.name, pos.line, pos.column, code, buf);
    } else {
        fprintf(stderr, "ERROR(%s:%u:%u): %s\n", pos.name, pos.line, pos.column, buf);
    }
    va_end(args);
    errorCollector.errorCount += 1;
}
