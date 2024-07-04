package com.example.multi_activity_app;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.example.multi_activity_app.R;

public class ImageRecycleAdapter extends RecyclerView.Adapter<ImageRecycleAdapter.ViewHolder> {
    Context context;
    int layout;
    String[] names;
    String[] roles;
    int[] imageIds;
    RecyclerViewInterface recyclerViewInterface;

    public ImageRecycleAdapter(Context context, int layout, String[] names, String[] roles, int[] imageIds,
                               RecyclerViewInterface recyclerViewInterface) {
        this.context = context;
        this.layout = layout;
        this.names = names;
        this.roles = roles;
        this.imageIds = imageIds;
        this.recyclerViewInterface = recyclerViewInterface;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(layout, parent, false);
        return new ViewHolder(view, recyclerViewInterface);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        holder.nameTextView.setText(names[position]);
        holder.roleTextView.setText(roles[position]);
        holder.icon.setImageResource(imageIds[position]);
    }

    @Override
    public int getItemCount() {
        return names.length;
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        TextView nameTextView;
        TextView roleTextView;
        ImageView icon;

        public ViewHolder(View itemView, final RecyclerViewInterface recyclerViewInterface) {
            super(itemView);
            nameTextView = itemView.findViewById(R.id.textViewName);
            roleTextView = itemView.findViewById(R.id.textViewRole);
            icon = itemView.findViewById(R.id.imageView);

            itemView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    if (recyclerViewInterface != null) {
                        int pos = getAdapterPosition();
                        if (pos != RecyclerView.NO_POSITION) {
                            recyclerViewInterface.onItemClick(pos);
                        }
                    }
                }
            });
        }
    }
}
