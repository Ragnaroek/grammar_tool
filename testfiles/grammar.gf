<Program> -> <ClassDeclarationList>
<ClassDeclarationList> -> ɛ
<ClassDeclarationList> -> <ClassDeclaration> <ClassDeclarationList>
<ClassDeclaration> -> class IDENT <Body>
<Body> -> extends IDENT { <ClassMemberList> }
<Body> -> { <ClassMemberList> }
<ClassMemberList> -> ɛ
<ClassMemberList> -> <ClassMember> <ClassMemberList>
<ClassMember> -> public <Type> IDENT <FieldOrMethod>
<ClassMember> -> public <MainMethod>
<FieldOrMethod> -> <Field>
<FieldOrMethod> -> <Method>
<MainMethod> -> static void IDENT ( IDENT [ ] IDENT ) <Block>
<Field> -> ;
<Method> -> ( ) <Block>
<Method> -> ( <Parameters> ) <Block>
<Parameters> -> <Parameter> <ParameterList>
<ParameterList> -> ɛ
<ParameterList> -> , <Parameters>
<Parameter> -> <Type> IDENT
<Type> -> int
<Type> -> boolean
<Type> -> void
<Type> -> IDENT
<Statement> -> <Block>
<Statement> -> <EmptyStatement>
<Statement> -> <IfStatement>
<Statement> -> <ExpressionStatement>
<Statement> -> <WhileStatement>
<Statement> -> <ReturnStatement>
<BlockStatementList> -> ɛ
<BlockStatementList> -> <BlockStatement> <BlockStatementList>
<Block> -> { <BlockStatementList> }
<BlockStatement> -> <Statement>
<BlockStatement> -> <LocalVariableDeclarationStatement>
<LocalVariableDeclarationStatement> -> <Type> IDENT <DeclOrDef>
<DeclOrDef> -> ;
<DeclOrDef> -> = <Expression> ;
<EmptyStatement> -> ;
<WhileStatement> -> while ( <Expression> ) <Statement>
<IfStatement> -> if ( <Expression> ) <Statement> <IfElseStatement>
<IfElseStatement> -> ɛ
<IfElseStatement> -> else <Statement>
<ExpressionStatement> -> <Expression> ;
<Expression> -> <AssignmentExpression>
<AssignmentExpression> -> <LogicalOrExpression>
<AssignmentExpression> -> <LogicalOrExpression> = <AssignmentExpression>
<LogicalOrExpression> -> <LogicalAndExpression>
<LogicalOrExpression> -> <LogicalAndExpression> || <LogicalOrExpression>
<LogicalAndExpression> -> <EqualityExpression>
<LogicalAndExpression> -> <EqualityExpression> && <LogicalAndExpression>
<EqualityExpression> -> <RelationalExpression>
<EqualityExpression> -> <RelationalExpression> == <EqualityExpression>
<EqualityExpression> -> <RelationalExpression> != <EqualityExpression>
<RelationalExpression> -> <AdditiveExpression>
<RelationalExpression> -> <AdditiveExpression> ? <RelationalExpression>
<RelationalExpression> -> <AdditiveExpression> ?= <RelationalExpression>
<RelationalExpression> -> <AdditiveExpression> ??= <RelationalExpression>
<RelationalExpression> -> <AdditiveExpression> ?? <RelationalExpression>
<AdditiveExpression> -> <MultiplicativeExpression>
<AdditiveExpression> -> <MultiplicativeExpression> + <AdditiveExpression>
<AdditiveExpression> -> <MultiplicativeExpression> - <AdditiveExpression>
<MultiplicativeExpression> -> <UnaryExpression>
<MultiplicativeExpression> -> <UnaryExpression> * <MultiplicativeExpression>
<MultiplicativeExpression> -> <UnaryExpression> / <MultiplicativeExpression>
<MultiplicativeExpression> -> <UnaryExpression> % <MultiplicativeExpression>
<UnaryExpression> -> <PrimaryExpression>
<UnaryExpression> -> ! <UnaryExpression>
<UnaryExpression> -> - <UnaryExpression>
<ReturnStatement> -> return ;
<ReturnStatement> -> return <Expression> ;
<PrimaryExpression> -> null <MIEorFAE>
<PrimaryExpression> -> false <MIEorFAE>
<PrimaryExpression> -> true <MIEorFAE>
<PrimaryExpression> -> INTEGER_LITERAL <MIEorFAE>
<PrimaryExpression> -> <LocalVariableReferenceExpression>
<PrimaryExpression> -> this <MIEorFAE>
<PrimaryExpression> -> ( <Expression> ) <MIEorFAE>
<PrimaryExpression> -> <NewObjectExpression> <MIEorFAE>
<PrimaryExpression> -> <FieldAccessExpression> <MIEorFAE>
<PrimaryExpression> -> <MethodInvocationExpression> <MIEorFAE>
<MethodInvocationExpression> -> IDENT ( <Content>
<MIEorFAE> -> . IDENT <Rest> <MIEorFAE>
<MIEorFAE> -> ɛ
<Rest> -> ( <Content>
<Rest> -> ɛ
<Content> -> )
<Content> -> <ExpressionList> )
<ExpressionList> -> <Expression> <Continuation>
<Continuation> -> ɛ
<Continuation> -> , <ExpressionList>
<FieldAccessExpression> -> IDENT
<LocalVariableReferenceExpression> -> IDENT
<NewObjectExpression> -> new IDENT ( )
