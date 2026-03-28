# The Role of Raw Power in Intelligence

**Hans Moravec**
**May 12, 1976**

[Original PDF from Stanford Digital Repository](https://stacks.stanford.edu/file/druid:ws563sd6050/ws563sd6050.pdf)

---

## Acknowledgement

The following entities provided inspiration, encouragement, suggestions, data, slave labor, proof reading services, etc.:

(in carefully randomized order) PDP-KA10, Scientific American, Marc Le Brun, Andy Moorer, Ed Mcguire, Electronics magazine, Don Gennery, Bill Gosper, John McCarthy, Macsyma, Mike Farmwald, Russ Taylor, Cart Project, Les Earnest, Pierre van Nypelseer, Robert Maas, Jeff Rubin, Bruce Baumgart, HAL-9000, Tom Binford, Clem Smith, Tom Gafford, Brian Harvey, ...

---

## Introduction

This essay is an argument that an essential ingredient is absent in many current conceptions of the road to Artificial Intelligence.

The first section discusses natural intelligence, and notes two major branches of the animal kingdom in which it evolved independently, and several offshoots. The suggestion is that intelligence need not be so difficult to construct as is sometimes assumed.

The second part compares the information processing ability of present computers with intelligent nervous systems, and finds a factor of one million difference. This abyss is interpreted as a major distorting influence in current work, and a reason for disappointing progress.

Section three examines the development of electronics, and concludes the state of the art can provide more power than is now available, and that the gap could be closed in a decade.

Parts four and five introduce hardware and software aspects of a system which is able to make use of the advancing technology.

---

## Section 1: The Natural History of Intelligence

### Product lines

Natural evolution has produced a continuum of complexities of behavior, from the mechanical simplicity of viruses to the magic of mammals. In the higher animals most of the complexity resides in the nervous system.

Evolution of the brain began in early multi-celled animals a billion years ago with the development of cells capable of transmitting electrochemical signals. Because neurons are more localized than hormones they allow a greater variety of signals in a given volume. They also provide evolution with a more uniform medium for experiments in complexity.

The advantages of implementing behavioral complexity in neural nets seem to have been overwhelming, since all modern animals more than a few cells in size have them [animal].

Two major branches in the animal kingdom, vertebrates and mollusks, contain species which can be considered intelligent. Both stem from one of the earliest multi-celled organisms, an animal something like a hydra made of a double layer of cells and possessing a primitive nerve net.

Most mollusks are intellectually unimpressive sessile shellfish, but one branch, the cephalopods, possesses high mobility, large brains and imaging eyes, evolved independently of the corresponding vertebrate structures. There are fascinating differences. The optic nerve connects to the back of the retina, so there is no blind spot. The brain is annular, forming a ring encircling the esophagus. The circulatory system, also independently evolved, has three blood pumps, a systemic heart pumping oxygenated blood to the tissues and two gill hearts, each pumping venous blood to one gill. The oxygen carrier is a green copper compound called hemocyanin, evolved from an earlier protein that also became hemoglobin.

These animals have some unique abilities. Shallow water octopus and squid are covered by a million individually controlled color changing effectors called chromatophores, whose functions are camouflage and communication. The capabilities of this arrangement have been demonstrated by a cuttlefish accurately imitating a checkerboard it was placed upon, and an octopus in flight which produced a pattern like the seaweed it was traversing, coruscating backward along the length of its body, diverting the eye from the true motion. Deep sea squid have photophores capable of generating large quantities of multicolored light. Some are as complex as eyes, containing irises and lenses [squid]. The light show is modulated by emotions in major and subtle ways. There has been little study of these matters, but this must provide means of social interaction. Since they also have good vision, there is the potential for high bandwidth communication.

Cephalopod intelligence has not been extensively investigated, but a few controlled experiments indicate rapid learning in small octopus [Boycott]. The Cousteau film shows an octopus' response to a "monkey and bananas" problem. A fishbowl containing a lobster is sealed with a cork and dropped into the water near it. The octopus is attracted, and spends a long while alternately probing the container in various ways and returning to its lair in iridescent frustration. On the final iteration it exits its little hole in the ground and unhesitatingly wraps three tentacles around the bowl, and one about the cork, and pulls. The cork shoots to the surface and the octopus eats. The Time-Life film contains a similar sequence, with a screw top instead of a cork! If small octopus have almost mammalian behavior, what might giant deep sea squid be capable of?

Birds are more closely related to humans than are cephalopods, their common ancestor with us being a 300 million year old early reptile. Size-limited by the dynamics of flying, some birds have reached an intellectual level comparable to the highest mammals.

Crows and ravens are notable for frequently outwitting man. Their intuitive number sense (ability to perceive the cardinality of a set without counting) extends to seven, as opposed to three or four in man. Such a sense is useful for keeping track of the number of eggs in a nest. Experiments have shown [Stettner] that most birds are more capable of high order "reversal" and "learning set" learning than all mammals except the higher primates. In mammals these abilities increase with increasing cerebral cortex size. In birds the same functions depend on areas not present in mammalian brains, forebrain regions called the "Wulst" and the hyperstriatum. The cortex is small and relatively unimportant. Clearly this is another case of independent evolution of similar mental functions.

It is interesting to speculate whether penguins, now similar to seals in behavior and habitat, will ever become fully aquatic, and evolve analogously to the great whales.

The cetaceans are related to man through a small 30 million year old primitive mammal. Some species of dolphin have body and brain masses identical to ours, and archaeology reveals they have been this way several times as long. They are as good as man at many kinds of problem solving, and perhaps at language. The references contain many anecdotes, and describe a few controlled experiments, showing that dolphins can grasp and communicate complex ideas. Killer whales have brains seven times human size, and their ability to formulate plans is better than dolphins', occasionally being used to feed on them. Sperm whales, not the largest animals, have the world's largest brains. There may be intelligence mediated conflict with large squid, their main food supply.

Elephants have brains about six times human size, matriarchal tribal societies, and complex behavior. Indian domestic elephants learn 500 commands, limited by the range of tractor-like tasks their owners need done, and form voluntary mutual benefit relationships with their trainers, exchanging labor for baths. They can solve problems such as how to sneak into a plantation at night to steal bananas, after having been belled (answer: stuff mud into the bells). And they remember for decades. Inconvenience and cost has prevented modern science from doing much work with them.

The apes are man's cousins. Chimps and gorillas can learn to use tools and to communicate with human sign languages at a retarded level. As chimps have one third, and gorillas one half, human brainsize, similar results should be achievable with the larger brained, but less man-like animals. Though no other species has managed to develop cultures comparable to modern man's, it may be that some of them can be made partners in ours, accelerating its evolution by their unique capabilities.

### Unifying principles

A feature shared by all living organisms whose behavior is complex enough to place them near human in intelligence is a hundred billion neuron nervous system. Imaging vision requires a billion neurons, while a million is associated with fast and interesting, but stereotyped, behavior as in a bee. A thousand is adequate for slow moving animals with minimal sensory input, such as slugs and worms. A hundred runs most sessile animals. The portions of nervous systems for which tentative wiring diagrams have been obtained, including much of the brain of the large neuroned sea slug, Aplysia, the flight controller of the locust and the early stages of some vertebrate visual systems, reveal that the neurons are configured into efficient, clever, assemblies. This should not be too surprising, as unnecessary redundancy means unnecessary metabolic load, a distinct selective disadvantage.

Evolution has stumbled on many ways of speeding up its own progress, since species that adapt more quickly have a selective advantage. Most of these, such as sex and individual death, have been refinements of one of the oldest, the encoding of genetic information in the easily mutated and modular DNA molecule. In the last few million years the genetically evolved ability of animals, especially mammals, to learn a significant fraction of their behavior after birth has provided a new medium for growth of complexity. Modern man, though probably not the most individually intelligent animal on the planet, is the species in which this cultural evolution seems to have had the greatest effect, making human culture the most potent force on the earth's surface.

Cultural and technological evolution proceeds by massive interchange of ideas and information, trial and error guided by the ability to predict the outcome of simple situations, and other techniques mediated by the intelligence of the participants. The process is self reinforcing because its consequences, such as improved communication methods and increased wealth and population, allow more experiments and faster cross fertilization among different lines of inquiry. Many of its techniques have not been available to biological evolution. The effect is that present day global civilization is developing capabilities orders of magnitude faster. Of course biological evolution has had a massive head start.

Although cultural evolution has developed methods beyond those of its genetic counterpart, the overall process is essentially the same. It involves trying large numbers of possibilities, selecting the best ones, and combining successes from different lines of investigation. This requires time and other finite resources.

Finding the optimum assembly of particular type of component which achieves a desired function usually requires examination of a number of possibilities exponential in the number of components in the solution. With fixed resources this implies a design time rising exponentially with complexity. Alternatively the resources can be used in stages, to design subassemblies, which are then combined into larger units, and so on, until the desired result is achieved. This can be much faster since the effort rises exponentially with the incremental size of each stage and linearly with the number of stages, with an additional small term, for overall planning, exponential in the number of stages. The resulting construct will probably use more of the basic component and be less efficient than an optimal design.

Biological evolution is affected by these considerations as much as our technology. Presumably there is a way of using the physics of the universe to construct entities functionally equivalent to human beings, but vastly smaller and more efficient. Terrestrial evolution has not had the time or space to develop such things. But by building in the sequence atoms, amino acids, proteins, cells, organs, animal (often concurrently), it produced a technological civilization out of inanimate matter in only two billion years.

### Harangue

The existence of several examples of intelligence designed under these constraints should give us great confidence that we can achieve the same in short order.

The situation is analogous to the history of heavier than air flight, where birds, bats and insects clearly demonstrated the possibility before our culture mastered it. Flight without adequate power to weight ratio is heartbreakingly difficult (vis. Langley's steam powered aircraft or current attempts at man powered flight), whereas with enough power (on good authority!) a shingle will fly. Refinement of the aerodynamics of lift and turbulence is most effectively tackled after some experience with suboptimal aeroplanes. After the initial successes our culture was able to far surpass biological flight in a few decades.

Although there are known brute force solutions to most AI problems, current machinery makes their implementation impractical. Instead we are forced to expend our human resources trying to find computationally less intensive answers, even where there is no evidence that they exist. This is honorable scientific endeavor, but, like trying to design optimal airplanes from first principles, a slow way to get the job done.

With more processing power, competing presently impractical schemes could be compared by experiment, with the outcomes often suggesting incremental or revolutionary improvements. Computationally expensive highly optimizing compilers would permit efficient code generation at less human cost. The expanded abilities of existing systems such as MACSYMA, along with new experimental results, would accelerate theoretical development. Gains made this way would improve the very systems being used, causing more speedup. The intermediate results would be inefficient kludges busily contributing to their own improvement. The end result is systems as efficient and clever as any designed by more theoretical approaches, but sooner, because more of the labor has been done by machines.

With enough power anything will fly. The next section tries to decide how much is needed.

---

## Section 2: Measuring Processing Power

### Low level vision

The visual system of a few animals has been studied in some detail, especially the layers of the optic nerve near the retina. The neurons comprising these structures are used efficiently to compute local operations like high pass filtering and edge, curvature, orientation and motion detection.

Assuming the whole optic nerve is as computationally intensive as the retina, successive layers producing increasingly abstracted representations, we can estimate the total capability. There are a million separate fibers in a cross section of the human optic nerve. At the retina each is connected to several of 10 million light sensitive rods and cones. The length of the nerve is a thousand times the depth occupied by the neurons which apply a single simple operation. The eye is capable of processing images at the rate of ten per second (flicker at higher frequencies is detected by special operators). This means that the optic nerve evaluates 10,000 million pixel simple operators each second.

A tightly hand coded simple operator, like high pass filtering by subtraction of a local average, applied to a 256x256 (1/16 million) pixel picture takes at least 10 seconds when executed on a PDP-10 (KA), not counting timesharing. A million pixel image would require 160 seconds, if storage constraints permitted it. Since the computer can evaluate only one at a time, the effective rate is 1/160 million pixel simple operators per second.

Thus a hand coded PDP-10 falls short of being the equal of the human optic nerve by a speed factor of 1.6 million.

It may not be necessary to apply every operator to every portion of every picture, and a general purpose computer, being more versatile than the optic nerve, can take advantage of this. I grant an order of magnitude for this effect, reducing the optic nerve to a mere 100,000 PDP-10 equivalents.

The size of this factor is related to having chosen to implement our algorithms in machine language. If we had opted to disassemble a number of PDP-10's and reconfigure the components to do the computation, far fewer would have been required. On the other hand if we had run our algorithms in interpreted Lisp, 10 to 100 times as many would be needed. The tradeoff is that the design time varies inversely with the execution efficiency. A good Lisp program to compute a given function is easier to produce than an efficient machine language program, or an equivalent piece of hardware. Compilers and automatic design programs blur the issue a little by passing some of the effort of implementation to a machine, but the essential character remains.

### Entropy measurement

Is there a quantitative way in which the processing power of a system, independent of its detailed nature, can be measured? A feature of things which compute massively is that they change state in complicated and unexpected ways. The reason for believing that, say, a stationary rock does little computing is its high predictability. By this criterion the amount of computing done by a device is in the mind of the beholder. A machine with a digital display which flashed 1, 2, 3, 4 etc., at fixed intervals would seem highly predictable to an adult of our culture, but might be justifiably considered to be doing an interesting, nontrivial and informative computation by a young child. Information theory provides a measure for this idea. If a system is in a given state and can change to one of a number of next states, the information in the transition, which I will call the Compute Energy (CE), is given by

> CE = -Sum(p_i * log(p_i)), for i = 1 to N

where N is the number of next states, and p_i is the probability that the i'th state will be occur next. If the base of the logarithm is two, the measure is in binary digits, bits. If we consider the system in the long run, considering all the states it might ever eventually be in, then this measure expresses the total potential variety of the system.

A machine which can accomplish a given thing faster is more powerful than a slower one. A measure for Compute Power is obtained by dividing each term in the above sum by the expected time of the corresponding transition. This is

> CP = Sum(-p_i * log(p_i) / t_i), for i = 1 to N

and the units are bits/second.

These measures are highly analogous to the energy and power capacities of a battery. Some properties follow:

They are linear, i.e. the compute power and energy of a system of two or more independent machines is the sum of the individual power and energies;

Speeding up a machine by a factor of n increases the CP by the same factor;

A completely predictable system has a CP and CE of zero;

A machine with a high short term CP, which can reach a moderate number of states in a short time, can yet have a low CE, if the total number of states attainable in the long run is not high;

If the probabilities and times of all the transitions are equal the measures simplify to

> CE = log_2(N)
>
> CP = log_2(N) / t

For N distinct outcomes and equal transition times the maximum CE and CP is obtained if all the probabilities of occurrence are 1/N, so the above simplifications represent an upper bound in cases where the probabilities are variable or cannot be determined.

### A representative computer

For the KA-PDP10, considering one instruction time, we have (roughly) that in one microsecond this machine is able to execute one of 2^5 different instructions, involving one of 2^4 accumulators and one of 2^18 memory locations, most of these combinations resulting in distinct next states. This corresponds to a CP of

> log_2(2^5 x 2^4 x 2^18) bit / 10^-6 sec = 27 x 10^6 bit/sec

This is an extreme upper bound, and represents the most efficient code possible. It cannot be maintained for very long, because many different sequences of instructions have the same outcome. Very often, for instance, the order in which a number of instructions is executed does not matter. Assuming for the moment that this is always the case, we can calculate the effect on the CP, measured over a second. The raw power says there are

> 2^(27 x 10^6)

distinct possible states after a second, or one million instruction times. If all permutations result in the same outcome, this must be reduced by a factor of 1000000!. The log of a quotient is the difference of the logs, so the adjusted CP is

> 27 x 10^6 - log_2(10^6!) = 27 x 10^6 - 18.5 x 10^6 = 8.5 x 10^6 bit/sec

The CP is also limited by the total compute energy. If we ignore external devices, this is simply the total amount of memory, about 36x2^18 = 9.4x10^6 bits. The PDP 10 could execute at its maximum effectiveness for 9.4/8.5 = 1.1 seconds before reaching a state which could have been arrived at more quickly another way.

Large external storage devices such as disks and tapes can increase the compute energy indefinitely, because they are a channel through which independently obtained energy may be injected. On the other hand, they have only a moderate effect on the power.

A disk channel connected to our KA-10 has a data rate of 6.4x10^6 bits/sec. If run at full speed, constantly stuffing new information into memory, it would add slightly less than that to the power, because it uses memory cycles the processor might want. If, as is usually the case, the disk is used for both reading and writing, the improvement is reduced by a factor of two. Further reductions occur if the use is intermittent. The overall impact is less than 10^6 bits/sec, about 10% of the power of the raw processor.

Combining the above results, we conclude that the processing power of a typical major AI center computer is at most 10^7 bits/sec. Time sharing reduces this to about 10^6 b/s per user. Programming in a moderately efficient high level language (vs. optimal machine code), costs another factor of 10, and running under an interpreter may result in a per user power of a mere 10,000 bits/sec, if the source code is efficient. These reductions can be explained in the light of the CP measure by noting that the a compiler or interpreter causes the executed code to be far more predictable than optimal code, eg. by producing stereotyped sequences of instructions for primitive high level constructs.

### A typical nervous system

We now consider the processing ability of animal nervous systems, using humans as an example. Since the data is even more scanty than what we assumed about the PDP-10, some not unassailable assumptions need to be made. The first is that the processing power resides in the neurons and their interconnections, and not in more compact nucleic acid or other chemical encodings. There is no currently widely accepted evidence for the latter, while neural mechanisms for memory and learning are being slowly revealed. A second is that the neurons are used reasonably efficiently, as detailed analysis of small nervous systems and small parts of large ones reveals (and common sense applied to evolution suggests). Thirdly, that neurons are fairly simple, and their state can be represented by a binary variable, "firing" or "not firing", which can change about once per millisecond. Finally we assume that human nervous systems contain about 40 billion neurons.

Considering the space of all possible interconnections of these 40 billion (treating this as the search space available to natural evolution in its unwitting attempt to produce intelligence, in the same sense that the space of all possible programs is available to someone trying to create intelligence in a computer), we note that there is no particular reason why every neuron should not be able to change state every millisecond. The number of combinations thus reachable from a given state is 2^(40x10^9) the binary log of which is 40x10^9. This leads to a compute power of

> 40 x 10^9 bit / 10^-3 sec = 40 x 10^12 bit/sec

which is about a million times the maximum power of the KA-10.

Keep in mind that much of this difference is due to the high level of interpretation in the 10, compared to what we assumed for the nervous system. Rewiring its gates or transistors for each new task would greatly increase the CP, but also the programming time. If the processor is made of 100,000 devices which can change state in 100 ns, the potential CP available through reconfiguration is 10^5 bits/10^-7 sec = 10^12 b/s. The CE would be unaffected. If automatic design and fabrication methods result in small quantity integrated circuit manufacture becoming less expensive and more widely practiced, my calculations may prove overly pessimistic.

### Thermodynamic efficiency

Thermodynamics and information theory provide us with a minimum amount of energy per bit of information generated at a given background temperature (the energy required to out shout the thermal noise). This is approximately the Boltzmann constant,

> 1.38 x 10^-16 erg/(deg molecule)

for our purposes the units will be rephrased as erg/(deg bit), since for normal matter molecules represent the limit of organization, due to the quantum mechanical fuzziness of things. This measure allows us to estimate the overall energy efficiency of computing engines. For instance, we determined the computing rate of the brain, which operates at 300 degrees K, to be 40x10^12 bits/sec. This corresponds to a physical power of

> 40 x 10^12 bit/sec x 300 deg x 1.38 x 10^-16 erg/(deg bit) = 1.66 erg/sec = 1.66 x 10^-7 watt

The brain runs on approximately 40 watts, so we conclude that it is 10^-8 times as efficient as the physical limits allow.

Doing the same calculation for the KA10, again at 300 deg, we see that a CP of 8.5x10^6 bit/sec is worth 3.519x10^-14 watts. Since this machine needs 10 kilowatts the efficiency is only 10^-18. Conceivably a ten watt, but otherwise equivalent, KA10 could be designed today, if care were taken to use the best logic for the required speed in every assembly. The efficiency would then be still only 10^-15.

As noted previously, there is a large cost inherent in the organization of a general purpose computer. We might investigate the computing efficiency of the logic gates of which it is constructed (as was, in fact, done with the brain measure). A standard TTL gate can change state in about 10ns, and consumes 10^-3 watt. The switching speed corresponds to a CP of 10^8 bit/sec, or a physical power of 4.14x10^-13 watt. So the efficiency is 10^-10, only one hundred times worse than a vertebrate neuron.

The newer semiconductor logic families are even better. C-MOS is twice as efficient as TTL, and Integrated Injection Logic is 100 times better, putting it on a par with neurons.

Experimental superconducting Josephson junction logic operates at 4 deg K, switches in 10^-11 sec, and uses 10^-7 watts per gate. This implies a physical compute power of 5x10^-12 watt, and an efficiency of 5x10^-5, 1000 times better than neurons. At room temperature it requires a refrigerator that consumes 100 times as much energy as the logic, to pump the waste heat uphill from 4 degrees to 300. Since the background temperature of the universe is about 4 degrees, this can probably eventually be done away with.

It is thus likely that there exist ways of interconnecting gates made with known techniques which would result in behavior effectively equivalent to that of human nervous systems. Using a million I^2L gates, or 10 thousand Josephson junction gates, and a trillion bits of slower bulk storage, all running at full speed, such assemblies would consume as little as, or less than, the power needed to operate a brain of the conventional type.

Past performance indicates that the amount of human and electronic compute power available is inadequate to design such an assembly within the next few years. The problem is much reduced if the components used are suitable large subassemblies. Statements of good high level computer languages are the most effective such modularizations yet discovered, and are probably the quickest route to human equivalence, if the necessary raw processing power can be accessed through them. This section has indicated that a million times the power of typical existing machines is required. The next suggests this should be available at reasonable cost in about ten years.

---

## Section 3: The Growth of Processing Power

The references below present, among other things, the following data points on a price curve:

| Year | Transistor Price | Milestone |
|------|-----------------|-----------|
| 1951 | $100 | transistor |
| 1952 | | transistor hearing aid |
| 1955 | | transistor radios |
| 1957 | $10 | transistor |
| 1960 | $1 | transistor |
| 1962 | | $100,000 small computer (IBM 1620) |
| 1965 | $0.08 | transistor (IC) |
| 1966 | | $1000 4 func calculator |
| 1967 | | $6000 scientific calc. |
| 1968 | | $10,000 small computer (PDP 8) |
| 1970 | | $200 4 func calculator |
| 1972 | | 1K RAMS (1 cent/bit) |
| 1974 | | $1000 small computer (PDP 11) |
| 1975 | | 4K RAMS (0.1 cent/bit) |
| 1976 | | $5 4 func calc (0.05 cent/trans) |

The numbers indicate a remarkably stable evolution. The price per electronic function has declined by a steady factor of ten every five years, if speed and reliability gains are included. Occasionally there is a more precipitous drop, when a price threshold which opens a mass market is reached. This makes for high incentives, stiff price competition and mass production economies. It happened in the early sixties with transistor radios, and is going on now for pocket calculators and digital wristwatches. It is begining for microcomputers, as these are incorporated into consumer products such as stoves, washing machines, televisions and sewing machines, and soon cars. During such periods the price can plummet by a factor of 100 in a five year period. Since the range of application for cheap processors is larger than for radios and calculators, the explosion will be more pronounced.

The pace of these gains is in no danger of slackening in the forseeable future. In the next decade the current period may seem to be merely the flat portion of an exponential rise. On the immediate horizon are the new semiconductor techniques, I^2L, and super fast D-MOS, CCD for large sensors and fast bulk memory, and magnetic bubbles for mass storage. The new 16K RAM designs use a folded (thicker) cell structure to reduce the area required per bit, which can be interpreted as the first step towards 3 dimensional integration, which could vastly increase the density of circuitry. The use of V-MOS, an IC technique that vertically stacks the elements of a MOS transistor is expanding. In the same direction, electron beam and X-ray lithography will permit smaller circuit elements.

In the longer run we have ultra fast and efficient Josephson junction logic, of which small IC's exist in an IBM lab, optical communication techniques, currently being incorporated into intermediate distance telephone links, and other things now just gleams in the eye of some fledgling physicist or engineer.

My favorite fantasies include the "electronics" of super-dense matter, either made of muonic atoms, where the electrons are replaced by more massive negative particles or of atoms constructed of magnetic monopoles which (if they exist) are very massive and affect each other more strongly than electric charges. The electronics and chemistry of such matter, where the "electron" orbitals are extremely close to the nucleus, would be more energetic, and circuitry built of it should be astronomically faster and smaller, and probably hotter. Mechanically it should exhibit higher strength to weight ratios. The critical superconducting transition field strengths and temperatures would be higher. For monopoles there is the possibility of combination magnetic electric circuitry which can contain, among many other goodies, DC transformers, where an electric current induces a monopole current at right angles to it, which in turn induces another electric current. One might also imagine quantum DC transformers, matter composed of a chainlike mesh of alternating orbiting electric and magnetic charges.

I interpret these things to mean that the cost of computing will fall by a factor of 100 during the next 5 years, as a consequence of the processor explosion, and by the usual factor of 10 in the 5 years after that. As an approximation to what is available today, note that in large quantities an LSI-11 sells for under $500. This provides a moderately fast 16 bit processor with 4K of memory. Another $500 could buy an additional 32K of memory, if we bought in quantity. The result would be a respectable machine, somewhat less powerful than the KA-10, for $1000. At the crude level of approximation employed in the previous section, a million machines of this type should permit human equivalence. A million dollars would provide a thousand of them today (a much better buy, in terms of raw processing power, than a million dollar large processor). In ten years a million dollars should provide the equivalent of a million such machines, in the form of a smaller number of faster processors, putting human equivalence within reach.

We now have the problem of what to do with this roomful of isolated computers. The next section describes what seems to be the best known solution to the problem of interconnecting a large number of processors with maximum generality, at reasonable cost.

---

## Section 4: Interconnecting Processors

The highest bandwidth and most flexible way for a number of computers to interact is by shared memory. Systems of the size considered here would require a large, but not unreasonable, address space of 100 billion words (40 bits of address). They would also demand memories with a thousand to a million ports. Although a variant of the method below could be used to construct such monsters, they would cost much more than the processors they served.

Alternatively, we might consider how a mass of the usual kind of machine, each with a moderate amount of local memory, could communicate through their bandwidth limited IO busses. Define a message period as the minimum interval in which a processor may transmit and receive one message. Assuming that messages are addressed to particular destination machines, an ideal interconnection system would allow every processor to issue a message during each message period, and deliver the highest priority message for each processor to it, providing notices of success or failure to the originators. The delay between transmission and reception of a message would be uniform and short, and the cost of the network would be reasonable (on the order of the cost of the processors served).

It so happens that this specification can be met. The following construction is a design due to K.E. Batcher [Batcher], extended considerably to permit pipelining of messages and higher speed.

### Log^2 sorting net construction

Batcher's major contribution is a method for constructing nets that can sort N numbers in log(N)^2 time, using only N x log^2(N) primitive two element sorters. Two nets plus a little additional circuitry can accomplish the message routing task.

The construction consists of a method of making a merger for two ordered lists of 2N numbers (call it a 2N-merger) from two N-mergers and 2N additional primitive elements. A primitive sorting element is a 1-merger.

An N-merger can thus be constructed from N x log_2(N) primitive elements. A sorter is made of a cascade of ever larger mergers, forming length two ordered lists from individual inputs with 1-mergers, combining these pairwise into length 4 lists with 2-mergers, and so on, until the entire sorted list of N numbers comes out of a final N/2-merger. The number of primitive elements involved is N x log_2(N) x (log_2(N) + 1) / 4.

### Communication scheme organization

The interconnection scheme is diagrammed in Figs 2, 3 and 4. Each processor is assigned a number, its "address", as indicated. In the sorters and the merger the smaller numbers come out towards the top of the diagram.

Messages are serial bit streams, and consist of a destination processor address, a priority number (invented by the originating computer), a one bit "dummy" flag field (set to 0 for actual messages), the address of the source processor (i.e. a return address), and the data to be communicated. A low priority number implies high priority. Zero is the highest priority.

The net is assumed to run at 100% duty cycle, with the processors emitting successive synchronized waves of messages. Every processor emits a message every message interval. The following discussion examines a single message wave.

The first sorting net orders the messages by destination address, and within a given destination by priority number. Thus the upper inputs of the merger receive a list of messages, grouped by destination, with the highest priority message to each processor heading its group.

The lower inputs of the merger receive N dummy messages, exactly one for each destination processor. The priority field is the highest possible (i.e. zero), the dummy flag is 1, the source address is the same as the destination, and the data portion is unused.

Merging these dummies with the sorted list of real messages results in a list still grouped by destination, with each group headed by a dummy, by virtue of its high priority, followed immediately by the highest priority real message, if any, for the destination.

This list is fed to the exchange network, which examines adjacent pairs of messages (considering overlapping pairs), and exchanges the data portions of a pair if the first member happens to be a dummy to a given address, and the second is a real message to the same address (i.e. it is the highest priority real message to that destination).

The sorting network following the exchanger sorts the messages by the field begining with the dummy flag, which acts as the high order bit, followed by the source address. Since there were N real messages, one from each processor, and N dummies, also nominally one from each processor, and since real messages are sorted ahead of dummy messages due to the high order bit, (i.e. the dummy flag) being 0, the second sorter restores the messages into the same order in which they were introduced.

Each processor has two input ports, one labeled "acknowledgement", and the other "incoming message". The acknowledgement input of processor i is connected to output i of the second sorter. The incoming message input is connected to output N+i (i.e. the i'th output of the lower half).

In the absence of the exchange network, the i'th processor would receive its own message back on its acknowledgement input, and the i'th dummy message on the incoming message input.

Because of the exchanger, however, if the message that processor i had sent happened to be the highest priority message to the requested destination, then the data portion of the message on the acknowledgement input would be that of the dummy it had been swapped with (signaling success). Also, if any messages had been addressed to processor i, the data portion of the highest priority one would arrive on the incoming message port, in place of the dummy message.

Thus a processor receives the highest priority message addressed to it on its incoming message port, or a dummy if nobody wanted to talk to it. It receives a dummy on its acknowledgement port if its message has gotten through, or the message back if it hasn't, due to the existence of a higher priority message to the same destination.

### Speed calculations

As outlined, a message consists of a destination address, a priority, a bit, a source address and a data portion. The two addresses must be at least large enough to uniquely specify each machine. Considering the case of a thousand and a million machine system, we note that the address lengths are 10 and 20 bits. Let's make the priority field the same length, leaving room for considerable flexibility in priority assignment schemes. The message portion should be fairly long, to permit messages like memory write requests, which require both a memory address and the data. Four address lengths, say. This gives us a message length 7 addresses long, 70 bits in a 1000 processor, 140 bits in a megaprocessor.

The full time taken by a message from start of transmission to completion of reception, in bit times, is the message length, plus the depth of the net (in primitive elements), plus an address and a priority time due to the buffering at the exchanger.

The depth of a 1024 processor net is 110 elements. This combines with the message and exchanger delays to result in a transit time of 110+70+20 = 200 bit times. If the bit rate is 100 MHz then messages are delivered in two microseconds. If the bit rate is 200 MHz, the time is 1 microsecond.

The same calculation for a megaprocessor reveals a depth of 420 and a total transit time of 420+140+40 = 600 bit times. This corresponds to 6 and 3 microsecond transits for 100 and 200 MHz data rates.

---

## Section 5: Programming Interconnected Processors

A major feature of the scheme outlined is its flexibility. It can function as any of the fixed interconnection patterns of the current lackluster multiprocessors, like Illiac IV, or as a hexagonal mesh, or a 7 dimensional cubic lattice, should that be desired, or the tree organization being considered in a Stanford proposal. It can simulate programmed pipeline machines, such as CDC Star, by using processors as arithmetic units. What is more, it can do all of these things simultaneously, since messages within one isolated subset of the processors have no effect on messages in a disjoint subset.

### A little Lisp

Take an existing Lisp and purify it to something closer to its lambda calculus foundations. Flush RPLACA and RPLACD and even SETQ (a pseudo SETQ can be introduced later), and discourage PROGs, which are inherently sequential and do things that could have been stated more clearly as recursive functions. In this system recursive programs are also more efficient.

The parallelism comes from the fact that the arguments in a multi-argument function can be evaluated simultaneously. This causes moderate parallelism when functions are nested, and can cause explosive parallelism when a function which at some level uses a multi-argument function, invokes itself recursively (as in a tree search). Most non-trivial programs stated as recursive functions do this.

### A little Algol

A simple construct which permits parallel execution is the "parallel semicolon", which we will indicate by a vertical bar "|". Statements separated by parallel semicolons may be executed simultaneously.

```
A := 3 |
B := 4 |
C := 6 ;
D := A+B+C ;
```

More massive concurrency can be obtained if the data types of Algol are expanded to include a complete set of array operators and genuine dynamic allocation of arrays. It is this type of data and operator set that makes APL an extremely powerful language in spite of an execrable control structure.

### Disclaimer

The software outlines are obviously only partly baked. This is mostly due to the limited amount of thought and work that has gone into them. On the other hand, it is my belief that even well thought out designs at this point will look naive in the light of experience with a working version of such a machine.

The flexibility offered should make this design much more attractive to a large class of programmers than current essentially special purpose architectures such as ILLIAC IV. Making it out of existing processors with proven machine languages will help too. I, for one, can hardly wait to start programming even a flawed version of a machine that can process and generate real time television with programs written in Algol, and simultaneously jump over tall game and proof trees in a single bound.
