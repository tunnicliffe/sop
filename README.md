# Sales Order Processing - prototype system

Test with: *stack build* and *stack test*

Remember to have a CouchDB set up with credentials matching those in **configs**, or the tests will fail at the database step.

## Design Philosophy

* Microservices, no dependencies, fast, simple as possible;
* One state store (CouchDB) to record order progress;
* Distributed services architecture (eg Docker + Kubernetes) 

A system of component APIs that contains of the following:
* Database
* Order Manager
* Suppliers Manager
* Supplier Adapters
* Warehouses Manager
* Deliverers Manager

## Current Approach

Haskell library _servant_ used to code RESTful APIs:

* Automatically generates query functions in Haskell **and** Javascript
* Automatically generates documentation
* Many existing 'cookbooks' on how to incorporate JWT, HTTPS, pagination, etc.
* Coding in Haskell leads to type-safety guarantees in URIs, both in their handling and contruction. Also gives good flexibility in error handling. Automatic query generation makes test code simple.

CouchDB used for order database:

* Document-store database paradigm suits this setting; Some orders may be more complex and require extra fields, and there isn't much need for relationships between orders
* CouchDB comes with its own RESTful API already, and deals in JSON (not just for the documents themselves, but for configurations, queries, and API responses)

## Code Layout and Logic

### CombinedM Monad for querying endpoints and handling those queries

Servant uses two monads for handling endpoints and querying them; *Handler = ExceptT ServerError IO*, and *ClientM = ReaderT ClientEnv (ExceptT ClientError IO)*. By combining them into a single *CombinedM = ReaderT Configs (ExceptT ServerError IO)*, we get numerous benefits:

1. Generated query functions can run in other handler functions without any transformation.
2. Errors from queries are in the ServerError type and can be automatically passed along all the way back to the user without any modification. 
3. Our handler functions have direct access to the configs, and therefore we can avoid any need to use the *Context* and *serveWithContext* functions of Servant.

The above is achieved in **Utils** through the use of conversion functions and Servant's monad hoisting capabilities.

### Query/Serve split to avoid cyclic dependency

It should be noted that APIs using eachother's query functions in their own handler functions causes a dependency between them. 
If that dependency is circular, then unfortunately this is currently a weakness of Haskell; It won't compile. 
Since they all only communicate through the Order Manager anyway, this problem has been solved in that component's code. 
By splitting it into **OrdManQuery.hs** and **OrdManServe.hs**, the query functions of the Order Manager can be at the root of the dependency tree, and the handler functions at the tip.

### Statelessness

All our component APIs, aside from the database, are stateless. 
All tasks require the Order Manager to query the database to obtain the full order document, before potentially updating it. 

### Basic Auth

Now done 'manually' on any given endpoint:
- Every endpoint checks for the 'Authorization' header, and so has a *Maybe ByteString* first parameter
- The handler code is wrapped by the *addAuthCheck* higher-order-function to automatically check for the header and its value
- The query function is wrapped by *removeAuthCheck* to simplify it for internal use in the components themselves (and never externally)

Now, each of our endpoints can be authorised to a custom degree. We gain security from external calls, but lose no convinience in the internal ones.

### WIP: Working around the rev field to unify SubmissionData and OrderData

The rev field can be obtained via the e-tag header using the HEAD method. 
When parsing a query of a whole doc, *fromJSON* will ignore the additional rev field.
When submitting, the rev field can be included as the rev query parameter, or in the If-Match request header.
Now with all this, we should be able to unify our two datatypes above.

### The rev field

CouchDB insists of the 'rev' field not being present during document creation, but then requires it for any subsequent updates. 
This prevents the easiest solution of having the 'rev' field inside the *OrderData* record type.
Instead, it is passed along with the 'If-Match' header. 
Note: The 'rev' query parameter approach was tried, but extra quotation marks were being added for some unknown reason.

### Testing and Logging

Running 'stack test' will execute the code in **Spec.hs**, automatically running and then querying each of the component APIs and their endpoints.
The Free Monad is used to examine the requests and responses directly.

However, intermediate communications are not available for inspection this way, for that purpose some form of logging is required.
This could be achieved through another Monad transformation effect (Writer), possibly in conjunction with the use of the Polysemy library. 
For now, logging is achieved through _maybeSetLogger_ which is a feature of the warp library. 
Each request and status code are recorded to a log file.

More detailed logging could be achived through adding the 'writer' monad effect. 
However, stacking too many monad transformations into the *CombinedM* has downsides.
More likely to implement this if we switch over to using the 'polysemy' library instead (see below).

In order to run each API on a different thread, perform tests as soon as they are all ready, and to close them again afterwards, some creative nesting of IO actions is used.

### Shared single ConnectionManager

The query functions all being *combinedM* allows them to be called directly in the handler code (which is also *combinedM*). 
Their actual execution requires a *ClientEnv*, which they get from the **Configs** via *createClieEnvFromLookup*.
But we don't actually want multiple *ConnectionManager* values being used by the same single handler. 
So instead of each datatype in the *IsAPI* class having a *conMan*, we just have a single one to refer to in the **Configs**.

In summary, each component API will use their own single *ConnectionManager* in the handling of its requests.

## Work in Progress

### OrderStatus needs an option for supplier failure

If they can't fulfil the order, say. This shouldn't make all of *notifySM* fail.

### Items may already be AtWarehouse

### Time to make DeliverersManager and TestDeliverer

### Test curls at the bottom of some components are incorrect

### Make two test suppliers to test item separation

TestSupplierA and TestSupplierB (and maybe two TestShop as well).

### deleteOrderOM doesn't make sense as an endpoint.

Remember to delete the test orders using deleteDocF in Spec.hs, since deleteOrderOM won't delete them once it's removed

Work towards 'amend' and 'cancel'. 

### Create a Docker image

https://docs.haskellstack.org/en/stable/docker_integration/

### Use higher-order-functions to clean up actionChain testing code

### Get working with HTTPS

See warp-tls package.

Can't use *defaultManagerSettings* anymore, need to use *tlsManagerSettings* (from http-client-tls).
BUT need to turn off the actual TLS check, so use *mkManagerSettings* with (*TLSSettingsSimple True False False*) from Network.Connection (connection).

Need to get CouchDB connection secure too.

### Documentation generation

See section in Servant docs

### Special types and or type integrity checks

GTINs have a specified form, for example. 
It should probably be the case that *submitOM* does a bunch of checking on what it recieves (beyond just parsing).

### Add the Writer monad transform (or effect) for logging?

Can logging be done through IO actions via warp settings?

In general, consider branching this project for polysemy to handle *CombinedM* instead.

In fact, why not adapt Servant to work with http-conduit (Network.HTTP.Simple seems nicer) and polysemy directly? 
I dislike this *ClientEnv* stuff; *parseBaseUrl* sucks

### Use toEncoding instead of toJSON

"about 3x faster, and less memory intensive besides".

Remember to change instances of *ToJSON* as well: https://hackage.haskell.org/package/aeson-1.4.6.0/docs/Data-Aeson.html#v:toEncoding

### Duplicate auth header bug

Problem isolated! applyBasicAuth is somehow causing the connection manager to add two duplicate basic auth headers:

* T 74.108.53.164:56344 -> 10.142.0.7:5984 [AP]
* PUT /test-db/sNWSfwTweh HTTP/1.1.
* Host: couch.oakeda.com:5984.
* Accept-Encoding: gzip.
* Content-Length: 118.
* Authorization: Basic Z3JpbWVzOnZpc2lvbnM=.
* Authorization: Basic Z3JpbWVzOnZpc2lvbnM=.
* Accept: application/json;charset=utf-8,application/json.
* Content-Type: application/json;charset=utf-8.
* .
* {"storeID":"NoPIBKfvgj","\_id":"sNWSfwTweh","customerName":"tIGOoXSSNh","deliveryAddress":"dnrsfDXHTg","productEAN":67}

And this causes CouchDB to throw 500! Note, the = in the header field may be the problem, as opposed to the duplicate nature.
Either way, I should submit two bug reports. One to http-client for applyBasicAuth's behaviour, and one to CouchDB.
The issue is NOT in how the credentials in the *RawConfigs* are parsed; They come out fine.

### Variable feedback problem

Some suppliers will reply more often than others.

Some suppliers will give more info than others.

Can we handle this just with *Maybe* types? 

Where do we encorporate estimated delivery times into our *OrderStatus*?

### XML adapter

The workflow seems to be:

1. servant-xml package provides XML as a valid content-type
2. xmlbf package provides FromXml, ToXml classes. fromXml :: Parser a, which you have to construct.
3. Then use runParser :: Parser a -> [Node] -> Either String a
4. To get the Node list, use fromRawXml :: ByteString -> Either String [Node] from either 'xmlbf-xeno' or 'xmlbf-xmlhtml'

I can do this.. but it may be highly specific to the use case.
It would be much nicer to use generic programming like Aeson does.. but it's hopelessly complicated.

Can generate client functions for interacting with an FTP server: https://hackage.haskell.org/package/ftp-client-0.5.1.3/docs/Network-FTP-Client.html .
I guess that will be enough.. but how often will I query it?

If everything is on their server: Repeating 'polling'

If they send to our server: Use iNotify with http://hackage.haskell.org/package/hinotify-0.4 <- cool stuff (what's the mac equivalent tho..? fswatch, fsnotify? What operating system are the Docker images going to use?)

NOTE: XML -> JSON conversion tools may exist. Should our parser fail, it may be useful to use one of these to get the data into the Value type, which we can then examine. 
