/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.dictionary;

import gnu.kawa.lispexpr.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.text.*;

import java.io.*;
import java.lang.reflect.*;
import java.util.*;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.dialogs.*;
import org.eclipse.jface.operation.*;
import org.eclipse.ui.*;

public class UserDictionary extends AbstractSymbolDictionary implements IResourceChangeListener {
    private static UserDictionary mInstance;
    private static List           mSchemeExtensions;
    
    private List      mPendingResources = Collections.synchronizedList(new LinkedList());
    private Hashtable mEntries = new Hashtable();

    private UserDictionary() {
        super(KawaDictionary.getInstance());
        initialize();
        ResourcesPlugin.getWorkspace().addResourceChangeListener(this, IResourceChangeEvent.POST_CHANGE);
    }

    public void dispose() {
        ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
    }

    public static UserDictionary getInstance() {
        if (mInstance == null) {
            mInstance = new UserDictionary();
        }
        return mInstance;
    }

    private void initialize() {
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        final IProject[] projects = root.getProjects();
        try {
            for(int i=0; i<projects.length; i++) {
                IProject project = projects[i];
                collectSchemeSources(project);
            }
            processPendingResources(false);
        }
        catch (Exception exception) {
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
                final IFile[] files = new IFile[mPendingResources.size()];
                synchronized (mPendingResources) {
                    mPendingResources.toArray(files);
                    mPendingResources.clear();
                }
                ProgressMonitorDialog dialog = null;
                try {
                    if (displayProgress)
                        dialog = new ProgressMonitorDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
                }
                catch (Exception exception) {
                    dialog = null;
                }
                if (dialog != null) {
                    dialog.run(true, true, new IRunnableWithProgress() {
                        public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
                            scanPendingChangedFiles(files, monitor);
                        }
                    });
                }
                else
                    scanPendingChangedFiles(files, null);
            }
            catch (Exception exception) {
                exception.printStackTrace();
            }
        }
    }
    
    private void scanPendingChangedFiles(final IFile[] files, IProgressMonitor monitor) {
        if (monitor != null)
            monitor.beginTask("SchemeScript - Updating symbol dictionary:", files.length);
        for (int i=0; i<files.length; i++) {
            IFile file = (IFile)files[i];
            try {
                file.deleteMarkers(IMarker.TEXT, false, 0);
            }
            catch (CoreException exception) {
                exception.printStackTrace();
            }
            scanSchemeFile(file, monitor);
            if (monitor != null)
                monitor.worked(1);
        }
        if (monitor != null)
            monitor.done();
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
                    position = ((PairWithPosition)object).getLine();
                }

                if ((car instanceof String) && (cdr instanceof Pair)) {
                    String name = ((String)car);
                    if (name.equals("define") || name.equals("defmacro") || name.equals("define-syntax")) { 
                        processDefineForm(cdr, resource, position, name);
                        continue;
                    }
                    if (name.equals("module-name")) {
                        processModulenameForm(cdr, resource, position);
                        continue;
                    }
                    if (name.equals("define-simple-class") || name.equals("define-class")) {
                        processSimpleClassForm(cdr, resource, position);
                        continue;
                    }
                }
            }
        }
    }

    private void processSimpleClassForm(Object cdr, IResource resource, int position) {
        Object cadr = ((Pair) cdr).car;
        if (cadr instanceof String) {
            String name = (String) cadr;
            mEntries.put(name, new SymbolEntry(name, "Class: " + name, SymbolEntry.VARIABLE, resource, position));
        }
    }

    private void processModulenameForm(Object cdr, IResource resource, int position) {
        Object cadr = ((Pair) cdr).car;
        if (cadr instanceof String) {
            String name = (String) cadr;
            mEntries.put(name, new SymbolEntry(name, "Module: " + name, SymbolEntry.VARIABLE, resource, position));
        }
    }

    private void processDefineForm(Object cdr, IResource resource, int position, String type) {
        Object cadr = ((Pair) cdr).car;
        if (cadr instanceof String) {
            String name = (String) cadr;
            String description = name;
            if (type.equals("define")) 
                description += " [variable]";
            else if (type.equals("defmacro") || type.equals("define-syntax")) 
                description += " [syntax]";
            mEntries.put(name, new SymbolEntry(name, description, SymbolEntry.VARIABLE, resource, position));
            return;
        }
        if (cadr instanceof Pair) {
            Object caadr = ((Pair) cadr).car;
            if (caadr instanceof String) {
                String description = cadr.toString();
                if (type.equals("defmacro") || type.equals("define-syntax")) 
                    description += " [syntax]";
                String name = (String) caadr;
                mEntries.put(name, new SymbolEntry(name, description, SymbolEntry.VARIABLE, resource, position));
            }
            return;
        }
    }

    protected void findSymbols(String name, List entries) {
        processPendingResources(true);
        SymbolEntry entry = (SymbolEntry) mEntries.get(name);
        if (entry != null)
            entries.add(entry);
    }

    protected void completeSymbols(String prefix, List entries) {
        processPendingResources(true);

        if (prefix.length() < 3) 
            return;
        
        Enumeration enumeration = mEntries.keys();
        while (enumeration.hasMoreElements()) {
            String symbolName = (String) enumeration.nextElement();
            if (symbolName.startsWith(prefix)) {
                entries.add((SymbolEntry) mEntries.get(symbolName));
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
                && (kind == IResourceDelta.ADDED || kind == IResourceDelta.CHANGED)
                && !((delta.getFlags() & IResourceDelta.MARKERS) != 0)) {
            processChangedResource((IFile)resource);
        }
        IResourceDelta[] children = delta.getAffectedChildren();
        for (int i = 0; i < children.length; i++) {
            processResourceDelta(children[i]);
        }
    }

    
    static {
        // TODO - should be made configurable
        mSchemeExtensions = new LinkedList();
        mSchemeExtensions.add("scm");
    }
}