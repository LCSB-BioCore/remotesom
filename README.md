
# `remotesom` -- Federated self-organizing maps

`remotesom` enables federated batch training of [Kohonen's self-organizing maps
(SOMs)](https://en.wikipedia.org/wiki/Self-organizing_map) on data points
scattered over the network / internet, using authenticated&encrypted TLS
communication. This allows you to create overview SOM-based maps and
clusterings (and many other things) of datasets without centralizing the data,
ideal when data transfer is impractical or legally restricted.

In essence, the training is split as follows:

- There are several nodes that host the data; these hosts compute local
  "summary" training commitments with respect to some given version of the SOM,
- an extra coordinator node maintains the "current" version of the SOM, and
  connects to the data hosts to gathers commitments used to iteratively train
  the SOM.

The SOM training proceeds as usual in the split-batch mode, where for each
batch:

- Each data host compute local "sums" and "counts" of their data points,
  grouped by the nearest-neighboring SOM node,
- the coordinator gathers all these summaries, applies the neighborhood
  function, updates the SOM, and repeats as needed with smaller neighborhood
  functions.

#### Acknowledgements

`remotesom` was developed at [Bioinformatics
Core](https://www.uni.lu/lcsb-en/research-groups/bioinformatics-core/) of
Luxembourg Centre for Systems Biomedicine, of University of Luxembourg.

<img src="media/unilu.svg" alt="Uni.lu logo" height="64px">   <img src="media/lcsb.svg" alt="LCSB logo" height="64px">

# How-to

In summary, you need to:

1. install `remotesom` on all hosts
2. prepare data on hosts, in a good binary format
3. generate cryptographic keys used for authentication
4. set up the network connections and start the data-host servers
5. launch the coordinator client for training

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

To feed this data to `remotesom`, you need to format it as an array of D×N
array of raw 32-bit `float`s. The data must be "row major", i.e., the data
should form groups of D `float`s each representing one of the N data points.

##### R

A matrix in R (variable `myMatrix`) with features in columns (which is the
usual setup) may be exported to file `mydata.bin` as follows:

```r
writeBin(as.vector(t(myMatrix)), "mydata.bin", size = 4)
```

##### Julia

Julia matrix (in variable `my_matrix`) with individual features in columns can
be exported to file `mydata.bin` in the correct format as follows:

```julia
open("mydata.bin", "w") do f
    write(f, Float32.(my_matrix'))
end
```

##### Python/NumPy

In Python, you can use
[`ndarray.tofile`](https://numpy.org/devdocs/reference/generated/numpy.ndarray.tofile.html)
to export a matrix in variable `my_matrix` to the correctly-formatted
`mydata.bin` as follows:

```python
import numpy as np

my_matrix.T.astype(np.float32).tofile("mydata.bin")
```


### Test the training locally (on data hosts)

It is possible to train the SOMs without any network, using command `remotesom
train`. For example, you can run a training of a small SOM on the above data as
follows:

```sh
remotesom train \
  -x 5 -y 5 -d "<DIMENSION D>" \
  -T out-test-topology.json -o out-test-som.json \
  -s 5 -s 4 -s 3 -s 2 -s 1 -s 0.5 \
  -D mydata.bin
```

This will output the trained SOM in `out-test-som.json`. The JSON file will
contain an array of arrays of numbers; each of the arrays represents one
feature vector that corresponds to one SOM centroid. The SOM topology is saved
in `out-test-topology.json` as a matrix of squared distances of the SOM nodes
in the map space (i.e., *not* centroids in data space), the JSON contains an
array of arrays of numbers, forming the columns of the all-to-all distance
matrix.

The command assumes the number of the data points from the size of `mydata.bin`
(as divided by the dimension in `-d` and the size of a single `float32`). To
avoid the extra "guessing" step, you can also specify exact data size using
option `-n`, such as `-n 10000`.

Before starting the federated training, it is advisable for all data hosts to
locally verify that their data is in a good shape by training and examining
such local SOM.

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

**NOTE:** There are more advanced options for authentication allowing to e.g.
use CA certificates for authentication, or omitting the authentication
completely for debugging or other practical purposes. See the output `remotesom
server --help` and `remotesom train-client --help`.

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
  -D mydata.bin -d "<DIMENSION D>" \
  -c server-cert.pem -k server-key.pem -a client-cert.pem
```
(The data dimension `-d` must be filled in, depending on the dataset;
optionally it is also adviseable to specify the datapoint count using `-n`. For
details about network&security parameters, see `remotesom server --help`.)

### Run the training (on coordinator)

Once data hosts are ready, the coordinator runs several epochs of SOM training:
```sh
remotesom train-client \
  -x 10 -y 10 -d "<DIMENSION D>" \
  -T out-topology.json -o out-som.json \
  -s 10 -s 9 -s 8 -s 7 -s 6 -s 5 -s 4 -s 3 -s 2 -s 1 -s 0.5 \
  -c client-cert.pem -k client-key.pem \
  connect datahost.uni1.example.org -a server-cert1.pem \
  connect hpc.uni2.example.org -a server-cert2.pem \
  ... # more data host connections
```
(The coordinator must fill in the data dimension `-D`, and the server certificates and
hostnames (and possibly other parameters) of all data hosts. See `remotesom
train-client connect --help` for all connection&security parameters.)

If everything runs well, the trained 10×10 SOM will appear in `out-som.json`.
For realistic data and larger SOMs, the number of training epochs will
typically need to be adjusted by adding more `-s` options.

### Compute per-cluster statistics (locally, on data nodes)

`remotesom stats` allows you to compute various statistics of the local data,
mainly mean and median values and datapoint count in centroid-defined clusters.

To generate all available cluster statistics from local data, run the following
command:

```sh
remotesom stats \
  -i out-test-som.json -D mydata.bin \
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
  -i out-som.json \
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

Sample code that plots the self-organizing map with some of the features and
counts statistics is provided below.

##### R

```r
library(jsonlite)
library(ggplot2)

topo <- read_json("out-test-topology.json", simplifyVector = T)
som <- read_json("out-test-som.json", simplifyVector = T)
counts <- read_json("counts.json", simplifyVector = T)

plotDimension <- 1  # choose a feature to plot here
df <- data.frame(
  x = topo$projection[, 1],
  y = topo$projection[, 2],
  feature = som[, plotDimension],
  count = counts
)

ggplot(df, aes(x, y, color = feature, size = count)) +
  geom_point() +
  scale_size_continous(trans = 'sqrt', range = c(0, 10))
```

##### Julia

```julia
using JSON, GLMakie

topo = JSON.parsefile("out-test-topology.json")
som = JSON.parsefile("out-test-som.json")
counts = JSON.parsefile("counts.json")

plot_dimension = 1  # choose a feature to plot here
positions = hcat(topo["projection"]...)'
features = hcat(som...)'
sizes = sqrt.(counts) ./ maximum(sqrt.(counts))

fig = Figure()
scatter(
    fig[1,1],
    positions[:, 1],
    positions[:, 2],
    color = features[:, plot_dimension],
    markersize = sizes .* 32, # size in makie points
    strokewidth = 0,
)
```

##### Python/NumPy

```python
import numpy as np
import json
import matplotlib.pyplot as plt

with open("out-test-topology.json", "r") as f:
  topo = json.load(f)
with open("out-test-som.json", "r") as f:
  som = json.load(f)
with open("counts.json", "r") as f:
  counts = json.load(f)

plot_dimension = 0  # choose a feature index to plot here
positions = np.array(topo["projection"])
features = np.array(som)
weights = np.array(counts) / max(counts)

plt.scatter(
  positions[:, 0],
  positions[:, 1],
  c = features[:, plot_dimension],
  s = weights * 144, # size in matplotlib points
  linewidth = 0,
)
plt.show()
```

### Subset the data to a region of interest

Quite often it is useful to reduce the data to one or more regions of interest
-- for example, this helps with removing outliers, and for clustering
interesting parts of the data in greater detail.

Once you have examined the SOM and identified the clusters that you want to
focus on, you can use `remotesom subset` to cut out these clusters of the data:

```sh
remotesom subset \
  -i out-test-som.json -D mydata.bin \
  -s 0 -s 1 -s 5 -s 10 \
  -O mysubset.bin
```

(You can add as many clusters to the subset, using the `-s` option as required.
The cluster numbers correspond exactly to the indexes of the array "centroids"
in the array stored in the SOM file (`out-test-som.json` in this case). Note
the clusters are numbered from zero!)

After finishing, the `subset` command prints out the total number of points
that are included in the subset. If desired, you can use that in subsequent
analysis as the new data size for the option `-n`:
```sh
remotesom train \
  [...]
  -D mysubset.bin -n "<NUMBER PRINTED BY SUBSET>" \
  [...]
```

##### Subsetting data on data hosts

The same can be performed also on data hosts -- the coordinator shares the SOM
and their choice of sub-clusters with the data hosts, each of whom may subset
their own part of the data individually. For convenience, the sub-cluster can be
also saved as JSON file, as a plain array of integers.

In this case, the coordinator shares the `out-som.json` and a file
`subset.json` that contains an array like this one (corresponds to the command
above):
```json
[0, 1, 5, 10]
```

Each data host then runs:
```sh
remotesom subset \
  -i out-som.json \
  -D mydata.bin -N "<DATA SIZE N>" \
  --subset-file subset.json \
  -O mysubset.bin
```

After the subsetting, the analysis may continue with data hosts serving the
`mysubset.bin` using `remotesom serve`, and the coordinator training and
analyzing a new SOM upon that.

# FAQ

#### Is the training reproducible?

Given the same architecture, the same data on data hosts, the same order of
data-host connections and the same initial training SOM, the training is
mathematically reproducible, and should not be a subject to floating-point
robustness errors.

To get a fixed initial SOM instead of a random one, include the `-R` parameter
with a seed in both the generating&training commands.
 ```sh
remotesom generate -R 12345 ...
remotesom train -R 12345 ...
remotesom train-client -R 12345 ...
```

Please open an issue if you detect any reproducibility issues caused by
floating-point robustness&rounding errors.

#### How much data can be processed at once?

##### Data size limits

In the current implementation, the data hosts do not load the input binary data
into memory, instead they use [mmap](https://linux.die.net/man/2/mmap) to map
the data of essentially any size without a direct RAM requirement. There are 2
limitations that are still present:

- **File size**: In general, the data size is N×D×4 bytes (where `N` is the
  number of points, `D` the dimensionality, and `4` the bytes per float). If
  your system have less RAM than that, the system won't be able to cache the
  whole file, and will thus have to periodically reload parts of it from the
  storage. In turn, the processing may get noticeably slower, but not
  impossible.
- **Internal data structures**: Some parts of the algorithms still need to
  materialize data-size-dependent arrays; most notably the array of "SOM
  centroid indexes that are closest to all points" is cached in several
  algorithms (notably, the local median computation and subsetting). This array
  usually requires N×8 bytes. With commonly available 16GB of memory, you can
  still process a hefty dataset of around 2 billion data points on a single
  host.

If you run out of memory because of the data size, you can split the dataset
into several data hosts without any impact on the result. In turn, this enables
horizontal scalability --- the speed & total amount of data processed at once
is only limited by the amount of computers you can attach to the analysis.

##### SOM size limits

The training coordinator node has to keep the full SOM topology in the memory.
Because we store a "generic" all-to-all distance topology file, this puts a
limit on the size of the self-organizing map that you can train. If the SOM
size is K nodes (typically, K=`grid-x`×`grid-y`), you need to fit (K²+K×D)×4
bytes into the memory.

On a modest computer with 16GB of memory, this thus allows you to train a SOM
of around 65 thousand centroids, which is a grid of roughly 250×250 centroids.
Typically, practical SOM grid sizes never exceed 100×100 (10 000 centroids),
and in for most cases 32×32 (1 024 centroids) is more than sufficient.

##### Data preparation limits

It is often the case that numeric computing environments can not manipulate
huge matrices, because they run out of memory. (Unless handled specially, this
is common in Julia, R, Python with NumPy, and many others.)

In such case, you can export many "parts" of the dataset independently, e.g.
like this in R:
```r
# ...prepare first part of data to myMatrix...
writeBin(as.vector(t(myMatrix)), "mydata1.bin", size = 4)
# ...prepare another part of the data into myMatrix...
writeBin(as.vector(t(myMatrix)), "mydata2.bin", size = 4)
```
...and concatenate them via command line into one big data blob:
```sh
cat mydata1.bin mydata2.bin > mydata.bin
```
(Similar method can be applied also in Julia, Python, and other environments.)

#### Can I access the cluster membership details programatically?

`remotesom subset` supports option `-M` that lets you export a vector of
cluster memberships for each input data point, which you can use to run
advanced statistics and subsetting on the data.

For convenience, there's also support for subsetting "related data"; e.g.,
metadata about points that were not included in the actual dataset (such as
`mydata.bin`), via options `-I` and `-b`.

Refer to `remotesom subset --help` for details.

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

Additionally, `remotesom` has out-of-the-box support for hexagonal, toroidal
and circular grid topologies. To get a complete list of the related options,
see the list of available options for the `generate` command:

```sh
remotesom generate --help
```

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

#### Can I use this to run usual k-means clustering?

Yes. Using a zero sigma degenerates the SOM batch-training epoch into a K-means
batch-training epoch. With options like `-s 0 -s 0 -s 0 ...` you thus
essentially run K-means clustering.

Note that K-means clustering requires careful initialization, which differs
from the random or grid-based initialization used by `remotesom generate`.
