/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.dictionary;

import gnu.expr.*;
import gnu.kawa.lispexpr.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.text.*;

import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.util.*;

import kawa.standard.*;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.operation.*;
import org.eclipse.jface.text.templates.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.*;

/**
 * User dictionary. Holds all the symbols defined by user code. All Scheme files
 * in all open projects are considered.
 * 
 * TODO Remove entries from removed resources (like when closing a project)
 * TODO Use a better technique to disable dictionaries...
 */
public class UserDictionary extends AbstractSymbolDictionary implements IUserDictionary, IResourceChangeListener {
    private List mSchemeExtensions;

    private List mPendingResources = Collections.synchronizedList(new LinkedList());
    private Hashtable mEntries = new Hashtable();
    private Hashtable mFormProcessors = new Hashtable();
    
    private static class FormProcessorDefiner extends Procedure2 {
        UserDictionary mDictionary;
        
        public FormProcessorDefiner(UserDictionary dictionary) {
            mDictionary = dictionary;
        }
        public Object apply2(Object arg0, Object arg1) {
            if (arg0 instanceof String && arg1 instanceof Procedure) {
                mDictionary.mFormProcessors.put(arg0, arg1);
            }
            return null;
        }
    }

    private UserDictionary(AbstractSymbolDictionary parent, String extensions) {
        super(parent);
        initializeExtensions(extensions);
    }

    public void dispose() {
        ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
        mEntries = null;
        mPendingResources = null;
    }

    public static UserDictionary createInstance(AbstractSymbolDictionary parent, String extensions, URL userFile) {
        UserDictionary instance = new UserDictionary(parent, extensions);
        instance.loadFormProcessors(userFile);
        instance.initialize();
        ResourcesPlugin.getWorkspace().addResourceChangeListener(instance, IResourceChangeEvent.POST_CHANGE);
        return instance;
}
    
    private void initializeExtensions(String extensions) {
        StringTokenizer tokenizer = new StringTokenizer(extensions, ",");
        mSchemeExtensions = new LinkedList();
        while (tokenizer.hasMoreElements()) {
            mSchemeExtensions.add(tokenizer.nextToken());
        }
    }

    private void loadFormProcessors(URL userFile) {
        Interpreter interp = Interpreter.getInterpreter();
        interp.defineFunction("define-form-processor", new FormProcessorDefiner(this));
        try {
            load.load.apply1(userFile.toString());
        }
        catch (Throwable exception) {
            SchemeScriptPlugin.logException("unable to load config file", exception);
        }
    }

    private void initialize() {
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        final IProject[] projects = root.getProjects();
        try {
            for (int i = 0; i < projects.length; i++) {
                IProject project = projects[i];
                collectSchemeSources(project);
            }
            processPendingResources(true);
        }
        catch (Exception exception) {
            SchemeScriptPlugin.logException("Error while updating user dictionary", exception);
        }
    }

    private void collectSchemeSources(IProject project) {
        try {
            IResource[] members = project.members();
            for (int elementIndex = 0; elementIndex < members.length; elementIndex++) {
                scanElement(members[elementIndex]);
            }
        }
        catch (Exception exception) {
        }
    }

    private void scanElement(IResource resource) throws CoreException {
        if (resource instanceof IFile) {
            IFile file = (IFile) resource;
            if (isSchemeSourceFile(file)) {
                synchronized (mPendingResources) {
                    mPendingResources.add(file);
                }
            }
        }
        else
            if (resource instanceof IContainer) {
                IContainer container = (IContainer) resource;
                IResource[] members = container.members();
                for (int elementIndex = 0; elementIndex < members.length; elementIndex++) {
                    scanElement(members[elementIndex]);
                }
            }
    }

    private void processPendingResources(boolean displayProgress) {
        if (!mPendingResources.isEmpty()) {
            try {
                final IFile[] files;
                synchronized (mPendingResources) {
                    files = new IFile[mPendingResources.size()];
                    mPendingResources.toArray(files);
                    mPendingResources.clear();
                }
                IWorkbenchWindow window = null;
                try {
                    if (displayProgress)
                        window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
                }
                catch (Exception exception) {
                    window = null;
                }
                if (window != null) {
                    window.run(true, true, new IRunnableWithProgress() {
                        public void run(IProgressMonitor monitor) throws InvocationTargetException,
                                InterruptedException {
                            scanPendingChangedFiles(files, monitor);
                        }
                    });
                }
                else
                    scanPendingChangedFiles(files, null);
            }
            catch (Exception exception) {
                SchemeScriptPlugin.logException("Error while updating user dictionary", exception);
            }
        }
    }

    private void scanPendingChangedFiles(final IFile[] files, IProgressMonitor monitor) {
        if (monitor != null)
            monitor.beginTask("Updating dictionary:", files.length);
        removeEntriesForFiles(files);
        for (int i = 0; i < files.length; i++) {
            IFile file = (IFile) files[i];
            scanSchemeFile(file, monitor);
            if (monitor != null)
                monitor.worked(1);
        }
        if (monitor != null)
            monitor.done();
    }

    private void removeEntriesForFiles(IFile[] files) {
        List removedEntries = new LinkedList();
        for (Iterator entryLists = mEntries.values().iterator(); entryLists.hasNext();) {
            List entriesForName = (List) entryLists.next();
            for (Iterator entries = entriesForName.iterator(); entries.hasNext();) {
                SymbolEntry entry = (SymbolEntry) entries.next();
                for (int i = 0; i < files.length; i++) {
                    if (entry.getFile() != null && entry.getFile().equals(files[i])) {
                        removedEntries.add(entry);
                        break;
                    }
                }
            }
        }
        for (Iterator iterator = removedEntries.iterator(); iterator.hasNext();) {
            removeEntry((SymbolEntry) iterator.next());
        }
    }

    private void scanSchemeFile(IFile file, IProgressMonitor monitor) {
        if (monitor != null) {
            monitor.subTask(file.getProject().getName() + " - " + file.getName());
        }
        File scmFile = file.getRawLocation().toFile();
        InPort port = null;
        try {
            try {
                port = new InPort(new FileReader(scmFile));
                ScmRead lexer = new ScmRead(port);
                scanExpressions(lexer, file);
            }
            finally {
                if (port != null)
                    port.close();
            }
        }
        catch (IOException exception) {
        }
        catch (SyntaxException exception) {
        }
    }

    private void scanExpressions(ScmRead lexer, IResource resource) throws IOException, SyntaxException {
        while (true) {
            Object object = lexer.readObject();
            if (object == EofClass.eofValue)
                return;

            if (object instanceof Pair) {
                Object car = ((Pair) object).car;
                Object cdr = ((Pair) object).cdr;
                int position = 0;
                if (object instanceof PairWithPosition) {
                    position = ((PairWithPosition) object).getLine();
                }

                if ((car instanceof String) && (cdr instanceof Pair)) {
                    String name = ((String) car);
                    Procedure formProcessor = (Procedure)mFormProcessors.get(name);
                    if (formProcessor != null) {
                        try {
                            formProcessor.applyN(new Object[] {this, object, resource, new Integer(position)});
                        }
                        catch (Throwable exception) {
                            SchemeScriptPlugin.logException("Unable to process form", exception);
                        }
                    }
                }
            }
        }
    }

    
    public void addEntry(SymbolEntry entry) {
        String name = entry.getName();
        List existingEntries = (List)mEntries.get(name);
        if (existingEntries == null) {
            existingEntries = new LinkedList();
            mEntries.put(name, existingEntries);
        }
        existingEntries.add(entry);
    }
    
    public void removeEntry(SymbolEntry entry) {
        String name = entry.getName();
        List existingEntries = (List)mEntries.get(name);
        if (existingEntries != null) {
            existingEntries.remove(entry);
        }
    }
    
    public Template[] completeTemplates(String prefix) {
        if (getParent() != null) 
            return getParent().completeTemplates(prefix);
        else
            return new Template[0];
    }
    
    public TemplateContextType getTemplateContextType()
    {
        if (getParent() != null) 
            return getParent().getTemplateContextType();
        else
            return null;
    }
    
    protected void findSymbols(String name, List entries) {
        processPendingResources(true);
        List existingEntries = (List) mEntries.get(name);
        if (existingEntries != null) {
            for (Iterator iterator = existingEntries.iterator(); iterator.hasNext();) {
                SymbolEntry entry = (SymbolEntry) iterator.next();
                entries.add(entry);
            }
        }
    }

    protected void completeSymbols(String prefix, List entries) {
        processPendingResources(true);

        Enumeration enumeration = mEntries.keys();
        while (enumeration.hasMoreElements()) {
            String symbolName = (String) enumeration.nextElement();
            if (symbolName.startsWith(prefix)) {
                List existingEntries = (List) mEntries.get(symbolName);
                for (Iterator iterator = existingEntries.iterator(); iterator.hasNext();) {
                    SymbolEntry entry = (SymbolEntry) iterator.next();
                    entries.add(entry);
                }
            }
        }
    }

    public void resourceChanged(IResourceChangeEvent event) {
        synchronized (mPendingResources) {
            processResourceDelta(event.getDelta());
        }
    }

    private void processChangedResource(IFile file) {
        try {
            if (isSchemeSourceFile(file)) {
                mPendingResources.add(file);
            }
        }
        catch (Exception exception) {
            SchemeScriptPlugin.logException("Error while updating user dictionary", exception);
        }
    }

    private boolean isSchemeSourceFile(IFile file) {
        String extension = file.getFileExtension();
        return extension != null && mSchemeExtensions.contains(extension);
    }

    private void processResourceDelta(IResourceDelta delta) {
        IResource resource = delta.getResource();
        int kind = delta.getKind();
        if (resource instanceof IFile
            && (kind == IResourceDelta.ADDED 
                || (kind == IResourceDelta.CHANGED 
                    && ((delta.getFlags() & IResourceDelta.CONTENT) != 0)))) {
            processChangedResource((IFile) resource);
        }
        IResourceDelta[] children = delta.getAffectedChildren();
        for (int i = 0; i < children.length; i++) {
            processResourceDelta(children[i]);
        }
    }
}