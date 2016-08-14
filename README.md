# `jupyter`: Jupyter ↔ Haskell

The `jupyter` package provides a type-safe high-level interface for interacting with Jupyter kernels
and clients using the [Jupyter messaging
protocol](https://jupyter-client.readthedocs.io/en/latest/messaging.html), without having to deal
with the low-level details of network communication and message encoding and decoding. Specifically,
the package provides a quick way of writing [Jupyter kernels](#creating-kernels) and [Jupyter
clients](#creating-clients) in Haskell.

If you are looking for a Haskell kernel (for evaluating Haskell in the Jupyter
notebook or another frontend), take a look at [IHaskell](http://github.com/gibiansky/IHaskell).

[![Build Status](https://travis-ci.org/gibiansky/jupyter-haskell.svg?branch=master)](https://travis-ci.org/gibiansky/jupyter-haskell)

### Table of Contents

- [What is Jupyter?](#what-is-jupyter)
- [Installation](#installation)
    * [ZeroMQ Installation](#zeromq-installation)
- [Intro to `jupyter`](#intro-to-jupyter)
- [Creating Kernels](#creating-kernels)
    * [Registering the Kernel](#registering-the-kernel)
    * [Communicating with Clients](#communicating-with-clients)
    * [A Complete Kernel](#a-complete-kernel)
    * [Reading from Standard Input](#reading-from-standard-input)
- [Creating Clients](#creating-clients)
    * [Finding Kernels](#finding-kernels)
    * [Establishing Communication with Kernels](#establishing-communication-with-kernels)
    * [Sending Requests to Kernels](#sending-requests-to-kernels)
- [Contributing](#contributing)
    * [Developer Notes](#developer-notes)


## What is Jupyter?

The [Jupyter project](http://jupyter.readthedocs.io/en/latest/) is a set of tools and applications
for working with interactive coding environments. Jupyter provides the architecture and frontends (also called clients),
and delegates the language-specific details to external programs called Jupyter kernels.

**Jupyter kernels** are interpreters for a language which communicate with Jupyter clients using
the [messaging protocol](https://jupyter-client.readthedocs.io/en/latest/messaging.html). The
messaging protocol consists primarily of a request-reply pattern, in which the kernel acts as a
server that responds to client requests. Clients can send requests to the kernel for executing code,
generating autocompletion suggestions, looking up documentation or metadata about symbols, searching
through execution history, and more.

**Jupyter clients** (also known as frontends) are programs that connect to kernels using the
messaging protocol, typically interacting with the kernels via a request-reply pattern. Clients 
can do whatever they want – currently, frontends exist for [notebook interfaces](http://jupyter.org/),
a [graphical terminal](https://github.com/jupyter/qtconsole), a [console interface](https://jupyter-console.readthedocs.io/en/latest/), [Vim plugins](https://github.com/ivanov/vim-ipython) for evaluating code, [Emacs modes](https://github.com/millejoh/emacs-ipython-notebook), and probably more.

The most commonly used and complex frontend is likely the [Jupyter notebook](http://jupyter.org/), which allows
you to create notebook documents with complex interactive visualizations,
Markdown documentation, code execution and autocompletion, and a variety of
other IDE-like features. You can try using the notebook with an [online demo
notebook](https://try.jupyter.org/).

The following screenshots are examples of the Jupyter notebook, taken from [their website](http://jupyter.org):
![Jupyter notebook](http://jupyter.org/assets/jupyterpreview.png)

## Installation

`jupyter` can be installed similarly to most Haskell packages, either via `stack` or `cabal`:
```
# Stack (recommended)
stack install jupyter

# Cabal
cabal install jupyter
```

### ZeroMQ Installation
`jupyter` depends on the `zeromq4-haskell` package, which requires ZeroMQ to be installed. Depending
on your platform, this may require compiling from source. If you use a Mac, it is recommended that
you install Homebrew (if you have not installed it already) and use it to install ZeroMQ.

Installation Commands:
- **Mac OS X (Homebrew, recommended):** `brew install zeromq`
- **Mac OS X (MacPorts):** `ports install zmq`
- **Ubuntu:** `sudo apt-get install libzmq3-dev`
- **Other:** Install ZeroMQ from source:
```bash
git clone git@github.com:zeromq/zeromq4-x.git libzmq
cd libzmq
./autogen.sh && ./configure && make
sudo make install
sudo ldconfig
```

## Intro to `jupyter`

The [Jupyter messaging protocol](https://jupyter-client.readthedocs.io/en/latest/messaging.html)
is centered around a request-reply pattern, with clients sending requests to kernels and kernels
replying to every request with a reply. Although that is the preliminary communication channel, there are five different messaging patterns that happen between clients and kernels:
- **Request-Reply**: Clients send requests to kernels, and kernels reply with precisely one response to every request.
- **Outputs**: Kernels broadcast outputs to all connected clients. (A client request can have only reply, as mentioned previously, but can also trigger any number of outputs being sent separately.)
- **Kernel Requests**: Kernels can send requests to individual clients; this is currently used *only* for querying for standard input, such as when the Python kernel calls `raw_input()` or the Haskell kernel calls `getLine`. (Requests for input are sent from the kernel to the client, and clients get the input from the user and send it to the kernel.)
- **Comms**: For ease of extension and plugin development, the messaging protocol supports `comm` channels; a `comm` is a one-directional communication channel that can be opened on either the kernel or client side, and arbitrary data may be sent by either the client to the kernel or the kernel to the client. This can be used for doing things that the messaging spec does not explicitly support, such as the [ipywidget support](https://github.com/ipython/ipywidgets) in the notebook.

The `jupyter` package encodes these messaging patterns in the type system. Each
type of message corresponds to its own data type, and clients and kernels are created by
supplying appropriate handlers for all messages that they can receive. For example, client requests correspond to the 
[`ClientRequest`](FAKE) data type:
```haskell
-- | A request sent by a client to a kernel.
data ClientRequest
  = ExecuteRequest CodeBlock ExecuteOptions
  | InspectRequest CodeBlock CodeOffset
  | CompleteRequest CodeBlock CodeOffset
  | ...

-- | A code cell
newtype CodeBlock = CodeBlock Text

-- | A cursor offset in the code cell
newtype CodeOffset = CodeOffset Int
```

Each of the [`ClientRequest`](FAKE) constructors has a corresponding [`KernelReply`](FAKE) constructor:
```haskell
-- | A reply sent by a kernel to a client.
data KernelReply
  = ExecuteReply ExecuteResult
  | InspectReply InspectResult
  | CompleteReply CompleteResult
  | ...
```

Kernels may send [`KernelOutput`](FAKE) messages to publish outputs to the clients:
```haskell
-- | Outputs sent by kernels to all connected clients
data KernelOutput
  = StreamOutput Stream Text
  | DisplayDataOutput DisplayData
  | ...

-- | Which stream to write to
data Stream = StreamStdout | StreamStderr

-- | 
newtype DisplayData = DisplayData (Map MimeType Text)

data MimeType
  = MimePlainText
  | MimeHtml
  | MimePng
  | ...
```

The other message types are represented by the [`KernelRequest`](FAKE) data type (for requests from
the kernel to a single client, e.g. for standard input), [`ClientReply`](FAKE) (for replies to
`KernelRequest` messages), and [`Comm`](FAKE) messages (for arbitrary communication between
frontends and servers).

## Creating Kernels

A kernel is an executable with distinct but related functions: first, registering the kernel with
Jupyter, and second, communicating via the messaging protocol with any connected clients.

### Registering the Kernel

The kernel must be able to register itself with the Jupyter client system on the user's machine, so
that clients running on the machine know what kernels are available and how to invoke each one.

The Jupyter project provides a command-line tool (invoked via `jupyter kernelspec install`) which
installs a kernel when provided with a directory known as a *kernelspec*. The `jupyter` project
automates creating and populating this directory with all needed files and invoking `jupyter
kernelspec install` via the [`installKernelspec`](FAKE) function in [`Jupyter.Install`](FAKE):

```haskell
installKernelspec :: InstallUser -- ^ Whether to install globally or just for this user.
                  -> Kernelspec  -- ^ Record describing the kernel.
                  -> IO InstallResult

-- | Install locally (with --user) or globally (without --user).
data InstallUser = InstallLocal | InstallGlobal

-- | Did the install succeed or fail?
data InstallResult = InstallSucccessful
                   | InstallFailed Text -- ^ Constructor with reason describing failure.
```

A kernel is described by a [`Kernelspec`](FAKE):
```haskell
data Kernelspec =
       Kernelspec
         { kernelspecDisplayName :: Text
         , kernelspecLanguage    :: Text
         , kernelspecCommand :: FilePath -> FilePath -> [String]
         , kernelspecJsFile :: Maybe FilePath
         , kernelspecLogoFile :: Maybe FilePath
         , kernelspecEnv :: Map Text Text
         }
```

The key required bits are the display name (what to call this kernel in UI elements), the language
name (what to call this kernel in code and command-line interfaces), and the command (how the kernel
can be invoked). The `kernelspecCommand` function is provided with the absolute canonical path to
the currently running executable and to a connection file (see the [next section](#communicating-with-clients)
for more on connection files), and must generate a command-line invocation.

For testing and demo purposes, `jupyter` provides a helper function [`simpleKernelspec`](FAKE) to generate a default kernelspec just from the three fields described above:
```haskell
simpleKernelspec :: Text  -- ^ Display name
                 -> Text  -- ^ Language name
                 -> (FilePath -> FilePath -> [String]) -- ^ Command generator
                 -> Kernelspec
```

Using [`simpleKernelspec`](FAKE), we can put together the smallest viable snippet for installing a kernelspec:

```haskell
-- | Install a kernel called "Basic" for a language called "basic".
--
-- The kernel is started by calling this same executable with the command-line 
-- argument "kernel" followed by a path to the connection file.
install :: IO ()
install = 
  let invocation exePath connectionFilePath = [exePath, "kernel", connectionFilePath]
      kernelspec = simpleKernelspec "Basic" "basic" invocation
  in installKernel InstallLocal kernelspec
```

### Communicating with Clients

Once the kernel is registered, clients can start the kernel, passing it a connection file as a
command-line parameter. A connection file contains a JSON encoded kernel profile
([`KernelProfile`](FAKE)), which specifies low-level details such as the IP address to serve on, the
transport method, and the ports for the ZeroMQ sockets used for communication.

To decode the JSON-encoded profile, the [`readProfile`](FAKE) utility is provided:
```haskell
-- | Try to read a kernel profile from a file; return Nothing if parsing fails.
readProfile :: FilePath -> IO (Maybe KernelProfile)
```

Obtaining the [`KernelProfile`](FAKE) enables you to call the main interface to the [`Jupyter.Kernel`](FAKE) module:
```haskell
serve :: KernelProfile -- ^ Specifies how to communicate with clients
      -> CommHandler   -- ^ What to do when you receive a Comm message
      -> ClientRequestHandler -- ^ What to do when you receive a ClientRequest
      -> IO ()
```

The kernel behaviour is specified by the two message handlers, the [`CommHandler`](FAKE) and the [`ClientRequestHandler`](FAKE). The [`ClientRequestHandler`](FAKE) receives a [`ClientRequest`](FAKE) and must generate a [`KernelReply`](FAKE) to send to the client:
```haskell
type ClientRequestHandler = KernelCallbacks -> ClientRequest -> IO KernelReply
```
The constructor of the output [`KernelReply`](FAKE) *must* match the constructor of the [`ClientRequest`](FAKE).

Besides generating the [`KernelReply`](FAKE), the [`ClientRequestHandler`](FAKE) may also send
messages to the client using the publishing callbacks:
```haskell
data KernelCallbacks = PublishCallbacks {
    sendKernelOutput :: KernelOutput -> IO (),
    sendComm :: Comm -> IO (),
    sendKernelRequest :: KernelRequest -> IO ClientReply
  }
```
For example, during code execution, the kernel will receive an [`ExecuteRequest`](FAKE), run the
requested code, using `sendKernelOutput` to send [`KernelOutput`](FAKE) messages to the client with
intermediate and final outputs of the running code, and then generate a [`ExecuteReply`](FAKE) that
is returned once the code is done running.

The [`CommHandler`](FAKE) is similar, but is called when the kernel receives [`Comm`](FAKE) messages
instead of [`ClientRequest`](FAKE) messages. Many kernels will not want to support any [`Comm`](FAKE) messages,
so a default handler [`defaultCommHandler`](FAKE) is provided, which simply ignores all [`Comm`](FAKE)
messages.

Unlike the [`CommHandler`](FAKE), the [`ClientRequestHandler`](FAKE) *must* generate a reply to
every request; it does not have the option of returning no output. Since there are quite a few
request types, a default implementation is provided as [`defaultClientRequestHandler`](FAKE), which
responds to almost all messages with an empty response:
```haskell
defaultClientRequestHandler :: KernelProfile -- ^ The profile this kernel is serving
                            -> KernelInfo    -- ^ Basic metadata about the kernel
                            -> ClientRequestHandler
```

The [`KernelInfo`](FAKE) record required for the [`defaultClientRequestHandler`](FAKE) has quite a
bit of information in it, so for any production kernel you will want to fill out all the records,
but for demo purposes `juptyer` provides the utility
[`simpleKernelInfo`](FAKE):
```haskell
simpleKernelInfo :: Text -- ^ Name to give this kernel
                 -> KernelInfo
```

Putting this all together, the shortest code snippet which runs a valid (but useless) Jupyter kernel
is as follows:
```haskell
runKernel :: FilePath -> IO ()
runKernel profilePath = do
  Just profile <- readProfile profilePath
  serve profile defaultCommHandler $
    defaultClientRequestHandler profile $ simpleKernelInfo "Basic"
```

### A Complete Kernel

Recall from the [registering the kernel section](#registering-the-kernel) that the kernel provides
an invocation to Jupyter; in our example, our kernel is invoked as `$0 kernel $CONNECTION_FILE`
(where `$0` is the path to the current executable). Our `runKernel` function from the previous
section must receive the file path passed as the `$CONNECTION_FILE`, so these two must be combined
in the same executable with a bit of flag parsing:

```haskell
main :: IO ()
main = do
  args <- getArgs
  case args of
    -- If invoked with the 'install' argument, then generate and register the kernelspec 
    ["install"] ->
      void $ installKernel InstallGlobal $
        simpleKernelspec "Basic" "basic" $ \exe connect -> [exe, "kernel", connect]

    -- If invoked with the 'kernel' argument, then serve the kernel
    ["kernel", profilePath] -> do
      Just profile <- readProfile profilePath
      serve profile defaultCommHandler $
        defaultClientRequestHandler profile $ simpleKernelInfo "Basic"
    _ -> putStrLn "Invalid arguments."
```

This example is available in the [`examples/basic`](FAKE) subdirectory, and you can build and run it
with `stack`:
```bash
stack build jupyter:kernel-basic
stack exec kernel-basic install
```
Once it is installed in this manner, you can run the kernel and connect to it from frontends; you
can try it with `jupyter console --kernel basic`. The kernel does not do much, though!

In order to write a more useful kernel, we would need to supply a more useful client request
handler; the client handler would need to parse the code being sent for execution, execute it, and
publish any results of the execution to the frontends using the `publishOutput` callback. An example
kernel that implements a simple calculator and handles most message types is provided in the
[`examples/calculator`](FAKE) directory, and can be built and run similarly to the `basic` kernel
(see above).

### Reading from Standard Input

In the Jupyter messaging protocol, the kernel never needs to send requests to the frontend, with the
exception of one instance: reading from standard input. Because knowing when standard input is read
requires executing code (something only the kernel can do), only the kernel can initiate reading
from standard input.

Since the kernel may be running as a subprocess of the frontend, or can even be running on a remote
machine, the kernel must be able to somehow intercept reads from standard input and turn them into
requests to the Jupyter frontend that requested the code execution. To facilitate this, the
[`KernelCallbacks`](FAKE) record provided to the [`ClientRequestHandler`](FAKE) has a
[`sendKernelRequest`](FAKE) callback:

```haskell
-- Send a request to the kernel and wait for a reply in a blocking manner.
sendKernelRequest :: KernelCallbacks -> KernelRequest -> IO ClientReply

-- Request from kernel to client for standard input.
data KernelRequest = InputRequest InputOptions
data InputOptions = InputOptions { inputPrompt :: Text, inputPassword :: Bool }

-- Response to a InputRequest.
data ClientReply = InputReply Text
```

The [`KernelRequest`](FAKE) and [`ClientReply`](FAKE) data types are meant to mirror the
more widely used [`ClientRequest`](FAKE) and [`KernelReply`](FAKE) data types; at the moment, these
data types are used only for standard input, but future versions of the messaging protocol may
introduce more messages.

An example of a kernel that uses these to read from standard input during code execution is
available in the [`examples/stdin`](FAKE) folder.

## Creating Clients

Jupyter clients (also commonly called frontends) are programs which connect to a running kernel
(possibly starting the kernel themselves) and then query them using the ZeroMQ-based messaging
protocol. Using the `jupyter` library, the same data types are used for querying kernels as for
receiving client messages, so if you understand [how to write kernels](#creating-kernels), using the
client interface will be straightforward.

### Finding Kernels

Any kernel that registers itself with Jupyter using `jupyter kernelspec install` (or the utilities
from the `jupyter` library) can then be located using `jupyter kernelspec list`. The `jupyter`
package provides two convenient wrappers around `jupyter kernelspec list`:

```haskell
-- Locate a single kernelspec in the Jupyter registry.
findKernel :: Text -> IO (Maybe Kernelspec)

-- List all kernelspecs in the Jupyter registry.
findKernels :: IO [Kernelspec]
```

Using the [`Kernelspec`](FAKE) and the `kernelspecCommand` field, we can find out how to launch any
registered kernel as a separate process. ([`System.Process`](FAKE) and the [`spawnProcess`](FAKE)
function may prove useful here.)

### Establishing Communication with Kernels

Before we can communicate with a kernel, we must first set up handlers for what to do when the
kernel sends messages to the client. The following handlers are required, stored in the
[`ClientHandlers`](FAKE) record:

```haskell
data ClientHandlers =
       ClientHandlers
         { kernelRequestHandler :: (Comm -> IO ()) -> KernelRequest -> IO ClientReply
         , commHandler :: (Comm -> IO ()) -> Comm -> IO ()
         , kernelOutputHandler :: (Comm -> IO ()) -> KernelOutput -> IO ()
         }
```

Each of the handlers receives a `Comm -> IO ()` callback; this may be used to send [`Comm`](FAKE)
messages to whatever kernel sent the message being handled.

* The `kernelRequestHandler` receives a [`KernelRequest`](FAKE) (likely a request for standard input
  from the user), and must generate an appropriate [`ClientReply`](FAKE), with a constructor
  matching the one of the request.
* The `commHandler` may do anything in response to a [`Comm`](FAKE) message, including doing
  nothing; since doing nothing is a common option, the [`defaultCommHandler`](FAKE) function is
  provided that does exactly this (that is, nothing).
* The `kernelOutputHandler` is called whenever a [`KernelOutput`](FAKE) message is produced by the
  kernel. This can be used to display output to the user, update a kernel busy / idle status
  indicator, etc.

Once a [`ClientHandlers`](FAKE) value is set up, the [`runClient`](FAKE) function can be used to run
any [`Client`](FAKE) command:

```haskell
runClient :: Maybe KernelProfile
          -> Maybe Username
          -> ClientHandlers
          -> (KernelProfile -> Client a)
          -> IO a
```

In addition to the handlers, [`runClient`](FAKE) takes an optional [`KernelProfile`](FAKE) to
connect to and an optional username. If no profile is provided, one is chosen automatically; if no
username is provided, a default username is used. The profile that was used (whether autogenerated
or set by the caller) is provided to an action `KernelProfile -> Client a`, and the [`Client`](FAKE)
action returned is run in `IO`.

### Sending Requests to Kernels

To send requests to kernels (and receive replies), construct the appropriate [`Client`](FAKE)
action; these are thin wrappers around `IO` that allow you to use the [`sendClientComm`](FaKE) and
more importantly [`sendClientRequest`](FAKE) to communicate with kernels. [`sendClientRequest`](FAKE)
can be used with any [`ClientRequest`], as in:

```haskell
getKernelInfoReply :: Client KernelInfo
getKernelInfoReply = do
  KernelInfoReply info <- sendClientRequest KernelInfoRequest
  return info
```

For example, the [`KernelInfo`](FAKE) for the `python3` kernel can be obtained as follows:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           System.Process (spawnProcess)

import           Jupyter.Client (runClient, sendClientRequest, ClientHandlers(..),
                                 defaultClientCommHandler, findKernel, writeProfile, Kernelspec(..))
import           Jupyter.Messages (ClientRequest(KernelInfoRequest), KernelReply(KernelInfoReply),
                                   KernelRequest(InputRequest), ClientReply(InputReply))

main :: IO ()
main = do
  -- Find the kernelspec for the python 3 kernel
  Just kernelspec <- findKernel "python3"

  -- Start the client connection
  runClient Nothing Nothing handlers $ \profile -> do
    -- Write the profile connection file to a JSON file
    liftIO $ writeProfile profile "profile.json"

    -- Launch the kernel process, giving it the path to the JSON file
    let command = kernelspecCommand kernelspec "" "profile.json"
    _ <- liftIO $ spawnProcess (head command) (tail command)

    -- Send a kernel info request and get the reply
    KernelInfoReply info <- sendClientRequest KernelInfoRequest
    liftIO $ print info

handlers :: ClientHandlers
handlers = ClientHandlers {
    -- Do nothing on comm messages
    commHandler = defaultClientCommHandler,

    -- Return a fake stdin string if asked for stdin
    kernelRequestHandler = \_ req ->
        case req of
        InputRequest{} -> return $ InputReply "Fake Stdin",

    -- Do nothing on kernel outputs
    kernelOutputHandler = \_ _ -> return ()
  }
```

This full example is in the [`examples/client-kernel-info` directory](FAKE), and can be built with `stack build`,
and executed with `stack exec client-kernel-info`. (It will work if the `python3` kernel is
installed, but not otherwise!)


## Contributing

Any and all contributions are welcome!

If you'd like to submit a feature request, bug report, or request for
additional documentation, or have any other questions, feel free to [file an
issue](http://github.com/gibiansky/jupyter-haskell/issues).

If you would like to help out, pick an
[issue](http://github.com/gibiansky/jupyter-haskell/issues) you are interested
in and comment on it, indicating that you'd like to work on it. Me (or other
project contributors) will try to promptly merge and pull requests and respond
to any questions you may have. If you'd like to talk to me (**@gibiansky**) off
of Github, feel free to email me; my email is available on my
[Github profile](https://github.com/gibiansky/) sidebar.

### Developer Notes
- **`stack build`**: Use `stack build` to build the library and run the examples.
- **`stack test`**: For any bug fix or feature addition, please make sure to
extend the test suite as well, and verify that `stack test` runs your test and
succeeds. [Travis CI](https://travis-ci.org/gibiansky/jupyter-haskell) is used to
test all pull requests, and must be passing before they will be merged.
- **`stack exec python python/tests.py`**: Part of the test suite is triggered
from Python (to be able to use the `jupyter_client` library); make sure that
the Python test suite passes as well. You can create a Python environment for
yourself separate from your global one with the `pyvenv` command: `pyvenv env
&& source env/bin/activate`.
- **`stack haddock`**: Please make sure that all top-level identifiers are well
documented; specifically, run `stack haddock` and ensure that all modules have
100% complete documentation. This is tested automatically on Travis CI, as well.
