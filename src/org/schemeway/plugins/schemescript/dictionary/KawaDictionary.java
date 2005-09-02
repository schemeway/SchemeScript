/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.dictionary;

import java.util.*;

public class KawaDictionary extends AbstractSymbolDictionary
{
    private static Hashtable mEntries = null;
    private static KawaDictionary mInstance; 

    private static SymbolEntry[] mKawaSymbols = {
       new SymbolEntry("module-name", "(module-name symbol)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       new SymbolEntry("module-static", "(module-static [boolean|'init-run])", SymbolEntry.SYNTAX),
       new SymbolEntry("module-export", "(module-exports symbol ...)", SymbolEntry.SYNTAX),
       new SymbolEntry("module-extends", "(module-extends <class>)", SymbolEntry.SYNTAX),
       new SymbolEntry("module-implements", "(module-implements <interface> ...)", SymbolEntry.SYNTAX),
       new SymbolEntry("require", "(require modulespec)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       new SymbolEntry("define-private", "(define-private name [:: <type>] value)", SymbolEntry.SYNTAX),
       new SymbolEntry("define-constant", "(define-constant name [:: <type>] value)", SymbolEntry.SYNTAX),
       new SymbolEntry("define-namespace", "(define-namespace name namespace-uri)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       new SymbolEntry("define-alias", "(define-alias name type)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       new SymbolEntry("define-simple-class", "(define-simple-class name (type ...) fields/methods ...)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       new SymbolEntry("define-class", "(define-class name (type ...) fields/methods ...)", SymbolEntry.SYNTAX),
       new SymbolEntry("object", "(object (type ...) fields/methods...)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       new SymbolEntry("make", "(make type args ...)", SymbolEntry.FUNCTION, SymbolEntry.MEDIUM),
       new SymbolEntry("instance?", "(instance? value type)", SymbolEntry.FUNCTION),

       new SymbolEntry("invoke", "(invoke object name args ...)", SymbolEntry.FUNCTION, SymbolEntry.MEDIUM),
       new SymbolEntry("invoke-static", "(invoke-static class name args ...)", SymbolEntry.FUNCTION, SymbolEntry.MEDIUM),
       new SymbolEntry("field", "(field object field-name)", SymbolEntry.FUNCTION, SymbolEntry.MEDIUM),
       new SymbolEntry("static-field", "(static-field class field-name)", SymbolEntry.FUNCTION, SymbolEntry.MEDIUM),
       new SymbolEntry("slot-ref", "(slot-ref object field-name)", SymbolEntry.FUNCTION, SymbolEntry.MEDIUM),
       new SymbolEntry("slot-set!", "(slot-set! object field-name value)", SymbolEntry.FUNCTION, SymbolEntry.MEDIUM),
       
       new SymbolEntry("try-catch", "(try-catch body handler ...)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       new SymbolEntry("try-finally", "(try-finally body handler)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       new SymbolEntry("primitive-throw", "(primitive-throw exception)", SymbolEntry.FUNCTION),
       new SymbolEntry("catch", "(catch key thunk handler)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       new SymbolEntry("throw", "(throw key args ...)", SymbolEntry.FUNCTION, SymbolEntry.MEDIUM),
       new SymbolEntry("error", "(error message args ...)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       
       new SymbolEntry("when", "(when condition form ...)", SymbolEntry.SYNTAX),
       new SymbolEntry("unless", "(unless condition form ...)", SymbolEntry.SYNTAX),
       
       new SymbolEntry("define-record-type", "(define-record-type name cstor pred? fields ...)", SymbolEntry.SYNTAX, SymbolEntry.MEDIUM),
       new SymbolEntry("defmacro", "(defmacro name body ...)", SymbolEntry.SYNTAX),

       new SymbolEntry("cond-expand", "(cond-expand [clause...] [(else command-or-definition...)])", SymbolEntry.SYNTAX),
       new SymbolEntry("gentemp", "(gentemp)", SymbolEntry.FUNCTION),
       new SymbolEntry("values-append", "(values-append arg ...)", SymbolEntry.FUNCTION),
       new SymbolEntry("receive", "(receive formals expression body)", SymbolEntry.SYNTAX),
       
       new SymbolEntry("keyword?", "(keyword? expr)", SymbolEntry.FUNCTION),
       new SymbolEntry("keyword->string", "(keyword->string keyword)", SymbolEntry.FUNCTION),
       new SymbolEntry("string->keyword", "(string->keyword string)", SymbolEntry.FUNCTION),
       
       new SymbolEntry("cut", "(cut slot-or-expr slot-or-expr* [<...>])", SymbolEntry.SYNTAX),
       new SymbolEntry("cute", "(cute slot-or-expr slot-or-expr* [<...>])", SymbolEntry.SYNTAX)
    };
                                                 
    private KawaDictionary()
    {
        super(R5RSDictionary.getInstance());
    }

    public static KawaDictionary getInstance() {
        if (mInstance == null) {
            mInstance = new KawaDictionary();
        }
        return mInstance;
    }

    
    protected void findSymbols(String name, List entries)
    {
        SymbolEntry entry = (SymbolEntry)mEntries.get(name);
        if (entry != null)
            entries.add(entry);
    }

    protected void completeSymbols(String prefix, List entries)
    {
        Enumeration enumeration = mEntries.keys();
        while (enumeration.hasMoreElements()) {
            String symbolName = (String) enumeration.nextElement();
            if (symbolName.startsWith(prefix)) {
                entries.add((SymbolEntry)mEntries.get(symbolName));
            }
        }
    }

    static {
        mEntries = new Hashtable();
        for (int index = 0; index < mKawaSymbols.length; index++)
        {
            SymbolEntry entry = mKawaSymbols[index];
            mEntries.put(entry.getName(), entry); 
        }
    }
}
