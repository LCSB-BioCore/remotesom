
# `remotesom` -- Federated self-organizing maps

`remotesom` runs batch training of [Kohonen's self-organizing maps
(SOMs)](https://en.wikipedia.org/wiki/Self-organizing_map) on data points
scattered over the network / internet, using authenticated&encrypted TLS
communication. This allows you to create overview maps and clusterings (and
many other things) of datasets even if the data can not be moved to a single
location (for practical or legal reasons).

In essence, the training is split as follows:

- There are several nodes that host the data; these compute "summary" training
  commitments with respect to some given version of the SOM,
- an extra coordinator node maintains the "current" version of the SOM, and
  connects to the data hosts to gathers commitments used to iteratively train
  the SOM.

The SOM training proceeds as usual in the split-batch mode, where for each
batch:

- The data hosts compute "sums" and "counts" of their data points, grouped by
  the nearest-neighboring SOM node,
- the coordinator gathers all such commitments, applies the neighborhood
  function, and continues with more iterations (and smaller neighborhood
  functions) as needed.

#### Acknowledgements

`remotesom` was developed at [Bioinformatics
Core](https://www.uni.lu/lcsb-en/research-groups/bioinformatics-core/) of
Luxembourg Centre for Systems Biomedicine, of University of Luxembourg.

<img src="media/unilu.svg" alt="Uni.lu logo" height="64px">   <img src="media/lcsb.svg" alt="LCSB logo" height="64px">

# How-to

In summary, you need to:

1. install `remotesom` on all hosts
2. prepare data on hosts, in a good binary format
3. prepare cryptographic keys used for authentication
4. prepare the network connections and start the data-host servers
5. run the coordinator client

### Installation

`remotesom` is best installed with `cabal` directly from the cloned repository:
```sh
cd remotesom/
git submodule update --init --recursive
cabal install
```

To install `cabal` packaging tool and generally the Haskell platform and
compiler, use [GHCup](https://www.haskell.org/ghcup/).

To compile successfully, you also need a working development installation of
LLVM-15. (To validate, check out if `llvm-config` command works for you and
gives the correct version 15.) LLVM can be usually obtained from OS packages,
typically under a name such as `llvm-15-dev` or similar. LLVM is required for
Accelerate framework to optimize the numeric code for your platform; for
additional details on the installation see [the appropriate section of the
documentation of
accelerate-llvm](https://github.com/AccelerateHS/accelerate-llvm/blob/master/README.md#installing-llvm).

### Prepare the data (on each data host)

For SOMs, you usually take a dataset of N data points represented by
D-dimensional feature vectors.

To feed this data to `remotesom`, you need to format it as an array of D×N raw
32-bit `float`s. The data must be "row major", i.e., the data should form
groups of D `float`s each representing one of the N data points.

For example, if you have a matrix in R with features in columns (which is the
usual setup), you can export it as follows:

```r
writeBin(as.vector(t(myMatrix)), "mydata.bin", size=4)
```

Other languages possess similar facilities; in Julia you can write binary data
using this code:
```julia
open("mydata.bin", "w") do f
    write(f, Float32.(my_matrix'))
end
```

With numpy, you can equivalently use
[`ndarray.tofile`](https://numpy.org/devdocs/reference/generated/numpy.ndarray.tofile.html).

### Test the training locally (on data hosts)

It is possible to train the SOMs without any network, using command `remotesom
train`. For example, you can run a training of a small SOM on the above data as
follows:

```sh
remotesom train \
  -x 5 -y 5 -d "<DIMENSION D>" \
  -T out-test-topology.json -o out-test-som.json \
  -s 5 -s 4 -s 3 -s 2 -s 1 -s 0.5 \
  -D mydata.bin -n "<DATA SIZE N>"
```

This will output the trained SOM in `out-test-som.json`. The JSON file will
contain an array of arrays of numbers; each of the arrays represents one
feature vector that corresponds to one SOM centroid. The SOM topology is output
in `out-test-topoology.json` as squared distances of the SOM nodes in the map
space (i.e., *not* the centroids in data space), the JSON contains array of
arrays of numbers which form columns of all-to-all distance matrix.

It is advisable for all data hosts to locally verify that their data is in a
good shape to train at least a local SOM before starting the federated
training.

### Prepare and exchange the keys (on all nodes)

Each data host generates a self-signed server certificate:

```sh
openssl req -x509 -newkey rsa:4096 -keyout server-key.pem -out server-cert.pem -sha256 -days 3650 -nodes -subj "/C=XX/ST=MyStateName/L=MyCityName/O=MyCompanyName/OU=MyCompanySectionName/CN=MyHostname"
```
(You should fill in all details of the certificate subject -- most importantly
the `CN` (Common Name) should match the hostname that the clients are going to
use to connect to your server. The hostname verification can be turned off, but
that is discouraged for any real use.)

The coordinator generates a self-signed client certificate:
```sh
openssl req -x509 -newkey rsa:4096 -keyout client-key.pem -out client-cert.pem -sha256 -days 3650 -nodes -subj "/C=XX/ST=MyStateName/L=MyCityName/O=MyCompanyName/OU=MyCompanySectionName/CN=MyName"
```
(The command is the same as for data hosts, but you don't need to care about
the `CN` common name at all.)

After the keys are generated, it is time to exchange the certificates
("identities") of all hosts.
- All data hosts send their **certificates** in file `server-cert.pem` to the
  coordinator
- The coordinator sends their **certificate** in file `client-cert.pem` to all
  data hosts.
- Remember that **NO ONE SENDS ANY PRIVATE KEYS.**

Notably, the transfer of certificates does not need to be encrypted (ownership
of an existing certificate does not give any advantage to a potential
attacker), but you should ensure that an attacker did not send you a crafted
certificate. If successful, the attacker might confuse you into accepting them
as a valid data host or coordinator, possibly exfiltrating secret data.
Sending the certificates via electronically signed e-mail, or even via any
authenticated instant-messaging service, is a sufficient protection against
such attacks.

*NOTE:* There are more advanced options for authentication allowing to e.g. use
CA certificates for authentication, or omitting the authentication completely
for debugging or other practical purposes. See the output `remotesom server
--help` and `remotesom train-client --help`.

### Prepare the network (on data hosts)

Data nodes must make sure that their server is reachable by direct TCP
connection from the coordinator. This can be achieved in several ways:

- use IPv6
- if the server is behind a NAT or firewall, use a SSH tunnel to forward the
  connection from some publicly accessible server to the data

The SSH forwarding typically uses the `ssh -R` approach. In the following
example, port 21012 on the remote server (HPC access nodes are great for
tunneling!) is forwarded to port 21012 on 127.0.0.1 (aka `localhost`), where
the `remotesom server` is expected to run.
```sh
ssh -N -R 21012:127.0.0.1:21012 myusername@myhpc-access.example.org
```
Refer to documentation of `ssh` for details.

### Start data host servers (on data hosts)

Each data host starts their own server by pointing it to the appropriate
cryptography keys and the local data source:

```sh
remotesom server \
  -D mydata.bin -n "<DATA SIZE N>" -d "<DIMENSION D>" \
  -c server-cert.pem -k server-key.pem -a client-cert.pem
```
(The data size and dimension must be filled in, depending on the dataset. See
`remotesom server --help` for all network&security parameters.)

### Run the training (on coordinator)

Once data hosts are ready, the coordinator runs several epochs of SOM training:
```sh
remotesom train-client \
  -x 10 -y 10 -d "<DIMENSION D>" \
  -T out-topology.json -o out-som.json \
  -s 10 -s 9 -s 8 -s 7 -s 6 -s 5 -s 4 -s 3 -s 2 -s 1 -s 0.5 \ # add more training epochs as needed
  -c client-cert.pem -k client-key.pem \
  connect datahost.uni1.example.org -a server-cert1.pem \
  connect hpc.uni2.example.org -a server-cert2.pem \
  ... # more data host connections
```
(The coordinator must fill in the data dimension D, and server certificates and
hostnames (and possibly other parameters) of all data hosts. See `remotesom
train-client connect --help` for all connection&security parameters.)

If everything runs well, the trained 10×10 SOM will appear in `som.json`.

### Compute per-cluster statistics (locally, on data nodes)

`remotesom stats` allows you to compute various statistics of the local data,
mainly mean and median values and datapoint count in centroid-defined clusters.

To generate all available cluster statistics from local data, run the following
comand:

```sh
remotesom stats \
  -i out-test-som.json \
  -D mydata.bin -n "<DATA SIZE N>" \
  --out-means means.json \
  --out-variances variances.json \
  --out-counts counts.json \
  --out-medians medians.json --median-min -1000 --median-max 1000 --median-iters 20
```

(The medians are computed by the interval-halving approximation algorithm, the
computation thus needs to know the initial interval and the iteration limit.)

### Compute per-cluster statistics remotely (on coordinator)

`remotesom stats-client` works exactly like `remotesom stats` but computes the
statistics from data stored on remote data nodes. (I.e., command `stats-client`
is to `stats` as `train-client` is to `train`.)

To compute the statistics with the SOM trained on the coordinator as above,
using the same data on remote data hosts, you can run the command as follows:

```sh
remotesom stats-client \
  -i out-test-som.json \
  --out-means means.json \
  --out-variances variances.json \
  --out-counts counts.json \
  --out-medians medians.json --median-min -1000 --median-max 1000 --median-iters 20 \
  -c client-cert.pem -k client-key.pem \
  connect datahost.uni1.example.org -a server-cert1.pem \
  connect hpc.uni2.example.org -a server-cert2.pem \
  ... # more data host connections
```

### Examine the results

`remotesom` does not provide any way to interpret or visualize the trained
SOMs; you need another data analysis environment to do that. The code examples
below may help you load and visualize the SOMs, potentially plugging the data
into other available packages.

Similar code can be used to examine the exported statistic (means, counts and
medians) from `remotesom stats` and `remotesom stats-client`.

##### R
```r
library(rjson)

# change the constants based on your data:
som_file <- "out-som.json"
x <- 10
y <- 10
dim <- 5

som <- array(do.call(cbind, fromJSON(file = som_file)), c(dim, x, y))

for(d in 1:dim)
  heatmap(som[d,,], Colv = NA, Rowv = NA)
```

##### Julia
```julia
using JSON, UnicodePlots

# change the constants based on your data:
som_file = "out-som.json"
x = 10
y = 10
dim = 5

som = reshape(hcat(JSON.parsefile(som_file)...), (dim,x,y))

for d in 1:dim
    heatmap(som[d,:,:])
end
```

# FAQ

#### Is the training reproducible?

Given the same architecture, same data on data hosts, same order of data-host
connections and same initial training SOM, the training is mathematically
reproducible, and should not be a subject to floating-point robustness errors.

To get a fixed initial SOM instead of a random one, use parameter `-R` to the
generating&training commands.

Please open an issue if you detect any reproducibility issues caused by
floating-point robustness&rounding errors.

#### How much data can be processed at once?

##### Data size limits

In the current implementation, the data hosts do not load the input binary data
into memory, instead they use [mmap](https://linux.die.net/man/2/mmap) to map
the data of essentially any size without a direct RAM requirement. There are 2
limitations that are still present:

- In general, the data size is N×D×4 bytes. If you have less memory than that,
  the system won't be able to cache the whole file, and will thus have to
  periodically reload parts of it from the storage. In turn, the processing may
  get noticeably slower, but not impossible.
- Some parts of the algorithms still need to materialize data-size-dependent
  arrays, most notably the array of "SOM centroid indexes that are closest to
  all points" is cached in the median computation to save computation time.
  This array requires (on `x86_64`) N×8 bytes. Still, with commonly available
  16GB of memory, you can still process a hefty dataset of around 2,000,000,000
  data points on a single host.

If you run out of memory because of the data size, you can split the dataset
into several data hosts without any impact on the result. In turn, this enables
horizontal scalability --- the speed & amount of data processed at once is only
limited by the amount of computers you can attach to the analysis.

##### SOM size limits

The training coordinator node has to keep the SOM topology in the memory.
Because we store a "generic" all-to-all distance topology file, this puts a
limit on the size of the self-organizing map that you can train. If the SOM
size is K nodes (typically, K=`som-x`×`som-y`), you need to fit (K²+K×D)×4
bytes into the memory. On a modest computer with 16GB of memory, this thus
allows you to train a SOM of around 65 thousand centroids, which is a grid of
roughly 250×250 centroids. Typically, practical SOM grid sizes never exceed
100×100, and in most cases 32×32 is more than sufficient.

##### Data preparation limits

It is often the case that numeric computing environments can not manipulate
huge matrices, because they run out of memory. (Unless handled specially, this
is common to Julia, R, Numpy, and many others.)

In such case, you can export many "parts" of the dataset independently, e.g.
like this in R:
```r
# ...prepare first part of data to myMatrix...
writeBin(as.vector(t(myMatrix)), "mydata1.bin", size=4)
# ...prepare another part of the data into myMatrix...
writeBin(as.vector(t(myMatrix)), "mydata2.bin", size=4)
```
...and concatenate them via commandline into one big data blob:
```sh
cat mydata1.bin mydata2.bin > mydata.bin
```
(Similar method can be applied in all languages.)

#### Can I use a different SOM topology?

Yes; you can input any SOM and any topology to the training commands using
options `-i` and `-t`. By default, files with the SOM and topology are
generated by `remotesom generate`:

- The SOM file is a JSON array of the total size of the SOM (i.e., X×Y
  "centroids" or "neurons") that contains numeric arrays of size D (feature
  vectors of each SOM centroid).
- The topology file is a JSON array or squared distances between each pair of
  neighbors, organized into an array that for each centroid contains vectors
  with **squared** distance to each centroid. (Viewed as a matrix, this would
  be a symmetric matrix with non-negative elements and zeroes on the diagonal.)

You can supply any data in a compatible format.

#### Can I use a different transport of the SOM training commitments than TLS?

Yes, you can organize the training and communication schedule yourself. The
individual steps can be performed manually without any communication by
commands:

1. `remotesom generate` to create an initial SOM and a topology (this is
   usually ran by the coordinator, who then sends the SOM to data hosts)
2. `remotesom summarize` to create a training commitment for a given SOM from
   local data (this is executed by data hosts)
3. `remotesom aggregate` which collects training commitments with topology and
   a neighborhood function, creating an updated SOM (this is typically ran by
   the coordinator)

Steps 2 and 3 are executed multiple times (each runs a single "epoch" of SOM
training with a different neighborhood function).

#### Can I use this to run usual clustering?

Using a zero sigma degenerates the SOM batch-training epoch into a K-means
batch-training epoch. With options like `-s 0 -s 0 -s 0 ...` you thus
essentially run K-means clustering.

Note that K-means clustering requires careful initialization (different than
what `remotesom generate` provides).
