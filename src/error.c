// NOTE: element #1 is the error code and element #2 is a short description of the error case.
// The description isn't used by the compiler itself but is there for tools to utilize
// in the future.
#define ERROR_CODES \
    FOR_EACH(TODO, "No individual error code exists for this error currently") \
    FOR_EACH(InvalidEscape, "Escape sequence is an invalid Unicode codepoint") \
    FOR_EACH(InvalidCharacterEscape, "Escape sequence is a invalid") \
    FOR_EACH(InvalidNumericEscape, "Escape sequence is an invalid Unicode codepoint") \
    FOR_EACH(InvalidCodePoint, "An invalid Unicode codepoint") \
    FOR_EACH(StringContainsNewline, "A string literal contains a newline") \
    FOR_EACH(UnexpectedEOF, "An unexpected EOF while parsing a string literal") \
    FOR_EACH(ExpectedDigit, "Expected a digit") \
    FOR_EACH(DigitOutOfRange, "Digit is out of range of base") \
    FOR_EACH(FloatOverflow, "Float literal overflow") \
    FOR_EACH(IntOverflow, "Integer literal overflow") \
    FOR_EACH(WrongDoubleQuote, "User entered `“` (0x201c) as a quote instead of ASCII") \
    FOR_EACH(Syntax, "Syntax error") \
    FOR_EACH(Fatal, "Fatal error") \
    FOR_EACH(Redefinition, "Redefinition") \
    FOR_EACH(MultipleConstantDecl, "Defined more than one constant item at a time") \
    FOR_EACH(ArityMismatch, "The amount of declarations doesn't match the amount of values") \
    FOR_EACH(NotAType, "A type was expected but something else was given") \
    FOR_EACH(UndefinedIdent, "Use of an undefined identifier") \
    FOR_EACH(InvalidConversion, "Unable to convert type to target type") \
    FOR_EACH(UninitImplicitArray, "Implicit-length array was provided without an initial value") \
    FOR_EACH(UninitFunctionType, "A function type wasn't provided a body") \
    FOR_EACH(TypeNotAnExpression, "A type is not a valid expression in the provided context") \
    FOR_EACH(NotNilable, "The type cannot be converted to nil") \
    FOR_EACH(InvalidPointeeType, "The pointee type is not a valid type") \
    FOR_EACH(ZeroWidthArrayElement, "Arrays with zero width elements are not permitted") \
    FOR_EACH(NonConstantArrayLength, "Array lengths must be constants") \
    FOR_EACH(ZeroWidthSliceElement, "Slices with zero width elements are not permitted") \
    FOR_EACH(ParamNameMissing, "The parameter doesn't have a name") \
    FOR_EACH(AddressOfNonAddressable, "The expr could not have it's address taken") \
    FOR_EACH(InvalidUnaryOperation, "Unary operation invalid for type") \
    FOR_EACH(InvalidBinaryOperation, "Binary operation invalid for type") \
    FOR_EACH(DivisionByZero, "Divided by zero") \
    FOR_EACH(TypeMismatch, "Type did not match") \
    FOR_EACH(BadCondition, "Expected a numeric or pointer type to act as a condition") \
    FOR_EACH(UnrepresentableValue, "The value could not be represented in the desired type without loss of information") \
    FOR_EACH(CastArgumentCount, "Cast did not receive exactly 1 expression") \
    FOR_EACH(AutocastExpectsDesiredType, "Context did not provide an expected type for autocast") \
    FOR_EACH(InvalidUseOfVoid, "Void can only exist as only result type for a function") \
    FOR_EACH(WrongNumberOfReturns, "Not enough return expressions") \
    FOR_EACH(AssignmentCountMismatch, "Left and right hand sides had a different number of values") \
    FOR_EACH(ValueNotAssignable, "Left hand value could not be assigned to") \
    FOR_EACH(NotAValue, "The expression provided is not a valid value") \
    FOR_EACH(ReferenceToDeclaration, "The Identifier references is being declared and is yet to be initialized") \
    FOR_EACH(GotoWithoutValue, "Goto statement requires a target") \
    FOR_EACH(BreakNotPermitted, "Break is not permitted outside of a switch or loop body") \
    FOR_EACH(ContinueNotPermitted, "Continue is not permitted outside of a loop body") \
    FOR_EACH(FallthroughNotPermitted, "Fallthrough is not permitted outside of a switch body") \
    FOR_EACH(FallthroughWithTarget, "Fallthrough statement cannot provide a target") \
    FOR_EACH(FallthroughWithoutNextCase, "Cannot fallthrough a case without a case following it") \
    FOR_EACH(CannotIterate, "Cannot iterate over a value of the given type") \
    FOR_EACH(CannotSwitch, "Cannot switch on type") \
    FOR_EACH(DefaultSwitchCaseNotLast, "The default switch case must also be the last case") \
    FOR_EACH(ArrayCompoundMissingIndex, "Array or slice compound literals expect indexes to be surrounded in `[]`") \
    FOR_EACH(InvalidSubscriptIndexType, "The type of the subscript index is not a valid index type") \
    FOR_EACH(UnsupportedSubscript, "Subscript is unsupported for the provided type") \
    FOR_EACH(OutOfBounds, "The subscript is out of bounds")

typedef enum ErrorCode {
#define FOR_EACH(e, s) e##Error,
    ERROR_CODES
#undef FOR_EACH
} ErrorCode;

typedef struct DiagnosticNote DiagnosticNote;
struct DiagnosticNote {
    const char *msg;
    DiagnosticNote *next;
};

typedef struct DiagnosticError DiagnosticError;
struct DiagnosticError {
    const char *msg;
    const char *sourceDescription;
    DiagnosticNote *note;
};

b32 shouldPrintErrorCode() {
    return FlagErrorCodes;
}

SourceRange rangeFromPosition(Position pos) {
    SourceRange range = {pos.name, pos.offset, pos.offset, pos.line, pos.column};
    return range;
}

#define HasErrors(p) (p)->diagnostics.errors

char *highlightLine(
                   char *buf, const char *bufend,
                   const char *line, const char *lineEnd,
                   const char *start, const char *end)
{

    ASSERT(line <= start);
    ASSERT(lineEnd >= end);
    int lengthRequired = 0;

    // Print line up until start into buf
    lengthRequired = snprintf(buf, bufend - buf, "%.*s", (int)(start - line), line);
    if (lengthRequired > bufend - buf) return NULL;
    buf += lengthRequired;
    line += lengthRequired;

    if (FlagErrorColors) {
        // Print highlight codes
        lengthRequired = snprintf(buf, bufend - buf, "\x1B[31m");
        if (lengthRequired > bufend - buf) return NULL;
        buf += lengthRequired;
    }

    ASSERT(line == start);

    // Print error range
    lengthRequired = snprintf(buf, bufend - buf, "%.*s", (int)(end - start), line);
    if (lengthRequired > bufend - buf) return NULL;
    buf += lengthRequired;
    line += lengthRequired;

    if (FlagErrorColors) {
        // Print reset code
        lengthRequired = snprintf(buf, bufend - buf, "\x1B[0m");
        if (lengthRequired > bufend - buf) return NULL;
        buf += lengthRequired;
    }

    // Print the remaining line
    lengthRequired = snprintf(buf, bufend - buf, "%.*s\n", (int)(lineEnd - line), line);
    if (lengthRequired > bufend - buf) return NULL;
    buf += lengthRequired;
    line += lengthRequired;

    // + 1 because we append a newline
    ASSERT(line == lineEnd + 1);

    return buf;
}

const char *findCodeBlockAndHighlightError(Package *p, SourceRange range) {

    // Find the file in the package matching range.name
    const char * fileStart = NULL;
    for (size_t i = 0; i < p->numFiles; i++) {
        if (strcmp(p->files[i].path, range.name) == 0) {
            fileStart = p->files[i].code;
            break;
        }
    }

    if (!fileStart) return NULL;

#define MAX_LINES 3
#define MAX_LINE_LENGTH 512

    // We add 1 so we have a buffer incase we have color turned off and need to add ^^^^^^ pointers.
    char lines[MAX_LINES + 1][MAX_LINE_LENGTH];
    char noColorHighlight[MAX_LINE_LENGTH];

    const char *cursor = &fileStart[range.offset];

    int numberOfBytes = 0;
    int numberOfLines = 0;
    for (int line = 0; line < MAX_LINES; line++) {
        const char *lineStart = cursor;
        const char *lineEnd = cursor;
        while(*cursor != '\n') {
            if (!isspace(*cursor)) lineStart = cursor;
            cursor--;
        }

        while(*lineEnd != '\n') lineEnd++;

        if (line != 0) {
            numberOfBytes += snprintf(lines[line], sizeof(lines[line]), "%.*s\n", (int)(lineEnd - lineStart), lineStart);
        } else {

            char *buf = &lines[line][0];

            u32 lineStartOffset = (u32) (lineStart - fileStart);
            u32 errorStartOffsetInLine = range.offset - lineStartOffset;
            u32 errorEndOffsetInLine = errorStartOffsetInLine + range.endOffset - range.offset;

            char *result = highlightLine(
               buf, buf + MAX_LINE_LENGTH,
               lineStart, lineEnd,
               lineStart + errorStartOffsetInLine, lineStart + errorEndOffsetInLine
            );
            if (!result) return NULL;
            numberOfBytes += strlen(buf);

            if (!FlagErrorColors) {
                memset(noColorHighlight, ' ', lineEnd - lineStart + 1);
                memset(noColorHighlight + errorStartOffsetInLine, '^', range.endOffset - range.offset);
                noColorHighlight[lineEnd - lineStart] = '\n';
            }
        }

        numberOfLines++;
        cursor--;
        
        if (*cursor == '\n') {
            // The line is empty, break
            break;
        }
    }

    numberOfBytes += 4 + 1; // 4 for indentation 1 for nul termination.
    if (!FlagErrorColors) numberOfBytes += strlen(lines[numberOfLines - 1]) + 1;
    char *results = ArenaAlloc(&p->diagnostics.arena, numberOfBytes + 1 + 40);

    char *resultsCursor = results;
    for (int line = numberOfLines - 1; line >= 0; line--) {
        resultsCursor += sprintf(resultsCursor, "\t%s", lines[line]);
    }
    if (!FlagErrorColors)
        resultsCursor += sprintf(resultsCursor, "\t%s", noColorHighlight);
    *resultsCursor = '\0';
    return results;
#undef MAX_LINES
}

void ReportError(Package *p, ErrorCode code, SourceRange range, const char *msg, ...) {
    va_list args;
    char msgBuffer[512];
    va_start(args, msg);
    vsnprintf(msgBuffer, sizeof(msgBuffer), msg, args);
    char errorBuffer[512];

    int errlen = shouldPrintErrorCode() ?
        snprintf(errorBuffer, sizeof(errorBuffer), "ERROR(%s:%u:%u, E%04d): %s\n", range.name, range.line, range.column, code, msgBuffer) :
        snprintf(errorBuffer, sizeof(errorBuffer), "ERROR(%s:%u:%u): %s\n",        range.name, range.line, range.column,       msgBuffer);

    // NOTE: snprintf returns how long the string would have been instead of its truncated length
    // We're clamping it here to prevent an overrun.
    errlen = MIN(errlen, sizeof(errorBuffer));

    char *errorMsg = ArenaAlloc(&p->diagnostics.arena, errlen + 1);
    memcpy(errorMsg, errorBuffer, errlen + 1);
    va_end(args);

    const char *codeBlock = NULL;
    if (FlagErrorSource) {
        codeBlock = findCodeBlockAndHighlightError(p, range);
    }

    DiagnosticError error = { errorMsg, codeBlock };
    ArrayPush(p->diagnostics.errors, error);
}

void ReportNote(Package *p, SourceRange pos, const char *msg, ...) {
    ASSERT(p->diagnostics.errors);
    va_list args;
    char msgBuffer[512]; // TODO: Static & Thread Local?
    va_start(args, msg);
    vsnprintf(msgBuffer, sizeof(msgBuffer), msg, args);
    char noteBuffer[512];

    int notelen = snprintf(noteBuffer, sizeof(noteBuffer), "NOTE(%s:%u:%u): %s\n", pos.name, pos.line, pos.column, msgBuffer);

    // NOTE: snprintf returns how long the string would have been instead of its truncated length
    // We're clamping it here to prevent an overrun.
    notelen = MIN(notelen, sizeof(noteBuffer));

    char *noteMsg = ArenaAlloc(&p->diagnostics.arena, notelen + 1);
    noteMsg = memcpy(noteMsg, noteBuffer, notelen + 1);
    va_end(args);

    DiagnosticNote *note = ArenaAlloc(&p->diagnostics.arena, sizeof(DiagnosticNote));
    note->msg = noteMsg;
    note->next = NULL;

    DiagnosticNote **indirect = &p->diagnostics.errors[ArrayLen(p->diagnostics.errors) - 1].note;
    while ((*indirect) != NULL)
        indirect = &(*indirect)->next;

    *indirect = note;
}

#if TEST
char outputErrorBuffer[8096];
#define outputDiagnostic(fmt, __VA_ARGS__) snprintf(outputErrorBuffer, sizeof(outputErrorBuffer), fmt, __VA_ARGS__)
#else
#define outputDiagnostic(fmt, __VA_ARGS__) fprintf(stderr, fmt, __VA_ARGS__)

#endif

void OutputReportedErrors(Package *p) {
    size_t numErrors = ArrayLen(p->diagnostics.errors);
    for (size_t i = 0; i < numErrors; i++) {
        outputDiagnostic("%s", p->diagnostics.errors[i].msg);
        if (p->diagnostics.errors[i].sourceDescription) {
            fprintf(stderr, "\n%s\n", p->diagnostics.errors[i].sourceDescription);
        }
        for (DiagnosticNote *note = p->diagnostics.errors[i].note; note; note = note->next) {
            outputDiagnostic("%s", note->msg);
        }
    }
    ArrayFree(p->diagnostics.errors);
    ArenaFree(&p->diagnostics.arena);
}
#undef OutputReportedErrorsPrinter

#if TEST
void test_errorReporting() {

    SourceRange builtinPosition = {0};
    Package mainPackage = {0};
    ReportError(&mainPackage, SyntaxError, builtinPosition, "Error Reporting value of five %d", 5);
    ReportNote(&mainPackage, builtinPosition, "Note Reporting value of six %d", 6);
    ASSERT(mainPackage.diagnostics.errors != NULL);
    OutputReportedErrors(&mainPackage);
    ASSERT(mainPackage.diagnostics.errors == NULL);
    ASSERT(mainPackage.diagnostics.arena.blocks == NULL);
}
#endif
