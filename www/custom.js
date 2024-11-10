function createMouseTrail() {
    const trails = 15; // Slightly reduced for better visibility
    const trailElements = [];
    
    for (let i = 0; i < trails; i++) {
        const trail = document.createElement('div');
        trail.className = 'mouse-trail';
        document.body.appendChild(trail);
        trailElements.push({
            element: trail,
            x: 0,
            y: 0,
            delay: i * 2
        });
    }

    let mouseX = 0;
    let mouseY = 0;
    let positions = Array(trails).fill({ x: 0, y: 0 });

    document.addEventListener('mousemove', (e) => {
        positions.unshift({ x: e.clientX, y: e.clientY });
        positions = positions.slice(0, trails);
    });

    function animate() {
        trailElements.forEach((trail, index) => {
            if (positions[index]) {
                trail.x = positions[index].x;
                trail.y = positions[index].y;

                trail.element.style.left = `${trail.x}px`;
                trail.element.style.top = `${trail.y}px`;

                // Increased starting size and slower size reduction
                const size = 8 - (index * 0.3); // Start at 8px, decrease more slowly
                const opacity = 0.8 - (index * 0.04); // Start at 0.8 opacity, decrease more slowly
                
                trail.element.style.width = `${Math.max(size, 1)}px`;
                trail.element.style.height = `${Math.max(size, 1)}px`;
                trail.element.style.opacity = Math.max(opacity, 0);
            }
        });

        requestAnimationFrame(animate);
    }

    animate();
}

document.addEventListener('DOMContentLoaded', createMouseTrail);