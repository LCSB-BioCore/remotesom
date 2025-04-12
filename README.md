
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
- the coordinator gathers all such commitments, applies the neigborhood
  function, and continues with more iterations (and smaller neighborhood
  functions) as needed.

## Acknowledgements

`remotesom` was developed at [Bioinformatics
Core](https://www.uni.lu/lcsb-en/research-groups/bioinformatics-core/) of
Luxembourg Centre for Systems Biomedicine, of University of Luxembourg.

<img src="media/unilu.svg" alt="Uni.lu logo" height="64px">   <img src="media/lcsb.svg" alt="LCSB logo" height="64px">

## How-to

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
typically under a name such as `llvm-15-dev` or similar.

### Prepare the data (on each data host)

For SOMs, you usually take a dataset of N data points represented by
D-dimensional feature vectors.

To feed this data to `remotesom`, you need to format it as an array of D×N raw
32-bit `float`s. The data must be "row major", i.e., the data should form
groups of D `float`s each representing one of the N data points.

If you have a matrix in R with features in columns (which is the usual setup),
you can export it as follows:

```r
writeBin(as.vector(t(myMatrix)), "mydata.bin", size=4)
```

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
tunelling!) is forwarded to port 21012 on 127.0.0.1 (aka `localhost`), where
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
(The data size and dimension must be filled in, depending on the dataset.)

### Run the training (on coordinator)

Once data hosts are ready, the coordinator runs several epochs of SOM training:
```sh
remotesom train-client \
	-x 10 -y 10 -d "<DIMENSION D>" -T topology.json -o som.json \
	-s 10 -s 9 -s 8 -s 7 -s 6 -s 5 -s 4 -s 3 -s 2 -s 1 -s 0.5 \
	-c client-cert.pem -k client-key.pem \
	-a server-cert1.pem -a server-cert2.pem ... \
	datahost.uni1.org dataserver.uni2.example.org ...
```
(The coordinator must fill in the data dimension D, and server certificates and
hostnames of all data hosts.)

If everything runs well, the trained 10×10 SOM will appear in `som.json`.
