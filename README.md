pvz-tdd-hs
==========

Plants vs Zombies in Haskell, via Test Driven Design

### The invariants of the Plants vs. Zombies game

* Zombie speed: 20cm per second
* Zombie speed after being hit by a snow pea: 10cm per second
* Projectile speed: 3m per second

#### Plant varieties:

<table>
<tr>
<td><img src="http://dserban.github.com/tmp/pvz/Gun.png"></td>
<td><strong>Regular Gun</strong><br>fires 1 regular projectile per second</td>
</tr>
<tr>
<td><img src="http://dserban.github.com/tmp/pvz/Repeater.png"></td>
<td><strong>Repeater Gun</strong><br>fires 2 regular projectiles per second</td>
</tr>
<tr>
<td><img src="http://dserban.github.com/tmp/pvz/SnowGun.png"></td>
<td><strong>Snow Gun</strong><br>fires 1 snow pea per second</td>
</tr>
</table>

Upon impact, each projectile removes 1 life from the leading zombie.
Projectiles do not reach any zombies who are behind the leading zombie.

A zombie's resilience is measured by the number of lives (a.k.a. how many
projectiles a zombie can absorb before dropping to the ground).

#### Zombie varieties:

<table>
<tr>
<td><img src="http://dserban.github.com/tmp/pvz/RegularZombie.png"></td>
<td><strong>Regular Zombie</strong><br>Resilience: 10 lives</td>
</tr>
<tr>
<td><img src="http://dserban.github.com/tmp/pvz/ConeheadZombie.png"></td>
<td><strong>Conehead Zombie</strong><br>Resilience: 28 lives</td>
</tr>
<tr>
<td><img src="http://dserban.github.com/tmp/pvz/BucketheadZombie.png"></td>
<td><strong>Buckethead Zombie</strong><br>Resilience: 65 lives</td>
</tr>
</table>

When a zombie arrives at a plant, it stops while devouring the plant, then
resumes walking. When devouring a plant, a zombie bites twice every second.
(After being hit by a snow pea, a zombie bites once every second.) A plant is
completely devoured after 6 zombie bites. While it's being devoured, a plant
continues to emit projectiles as per usual, and the zombie devouring the plant
keeps absorbing those projectiles.

### The problem statement

Pick one plant variety and one zombie variety from the above selections.  
For a given initial distance between the plant and the zombie, test-drive a program that determines who wins.  
If the plant and the zombie are too close to one another at the start of the game, the zombie is resilient enough to walk up to the plant and devour it completely - **the zombie wins**.  
If the plant and the zombie are far enough apart, there is plenty of time for the plant to shoot as many projectiles as it takes to kill the zombie from a distance - **the plant wins**.  
Therefore an equilibrium distance exists, where the plant wins, but just barely. What is that distance? Test-drive a program that you can use to manually find the answer by trial and error.
