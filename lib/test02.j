.class public test02
.super java/lang/Object

.method public <init>()V
    aload_0
    invokespecial java/lang/Object/<init>()V
    return
.end method

.method public static main([Ljava/lang/String;)V
.limit stack 3
.limit locals 1
    getstatic  java/lang/System/out Ljava/io/PrintStream;
    iconst_2
    bipush 44
    swap
    isub
    invokevirtual  java/io/PrintStream/println(I)V
    return
.end method
