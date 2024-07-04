package com.example.multi_activity_app;

import java.io.Serializable;

public class TeamMember implements Serializable {
    private String name, role, photo, bio, stats, link;

    public TeamMember(String name, String role, String photo, String bio, String stats, String link) {
        this.name = name;
        this.role = role;
        this.photo = photo;
        this.bio = bio;
        this.stats = stats;
        this.link = link;
    }

    // Getters and setters for each field
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public String getRole() { return role; }
    public void setRole(String role) { this.role = role; }

    public String getPhoto() { return photo; }
    public void setPhoto(String photo) { this.photo = photo; }

    public String getBio() { return bio; }
    public void setBio(String bio) { this.bio = bio; }

    public String getStats() { return stats; }
    public void setStats(String stats) { this.stats = stats; }

    public String getLink() { return link; }
    public void setLink(String link) { this.link = link; }
}
